;;; javacomp.el --- Java completion engine client -*- lexical-binding: t -*-

;; Copyright (C) 2017 Caibin Chen.
;; Copyright (C) 2015-2016 tide.el authors.

;; Author: Caibin Chen <tigersoldi@gmail.com>
;; URL: http://github.com/tigersoldier/javacomp-el
;; Version: 0.0.1
;; Keywords: java
;; Package-Requires: ((dash "2.10.0") (flycheck "27") (cl-lib "0.5") (projectile "0.14.0"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'etags)
(require 'json)
(require 'cl-lib)
(require 'eldoc)
(require 'dash)
(require 'flycheck)
(require 'imenu)
(require 'projectile)
(require 'thingatpt)

(defgroup javacomp nil
  "JavaComp."
  :prefix "javacomp-"
  :group 'tools)

(defcustom javacomp-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'javacomp)

(defcustom javacomp-server-jar "~/bin/JavaComp_deploy.jar"
  "Name of JavaComp jar file to run instead of the bundled JavaComp server jar.

This may either be an absolute path or a relative path. Relative
paths are resolved against the project root directory."
  :type '(choice (const nil) string)
  :group 'javacomp)

(defcustom javacomp-java-executable "java"
  "Name or path of the java executable binary file."
  :type '(choice (const nil) string)
  :group 'javacomp)

(defcustom javacomp-java-options nil
  "List of command line options to be pased to java command."
  :type '(set string)
  :group 'javacomp)

(defcustom javacomp-options-log-path ""
  "Server option for the path of the log file.

If it's empty, the server doesn't write any logs to file."
  :type 'string
  :group 'javacomp)

(defcustom javacomp-options-log-level nil
  "Server option for the path of the log level."
  :type '(choice (const nil)
                 (const "severe")
                 (const "warning")
                 (const "info")
                 (const "fine")
                 (const "finer")
                 (const "finest"))
  :group 'javacomp)

(defcustom javacomp-options-ignore-paths nil
  "Server option for a list of relative paths to be ignored."
  :type '(set string)
  :group 'javacomp)

(defcustom javacomp-options-type-index-files nil
  "Server option for a list of type index files."
  :type '(set string)
  :group 'javacomp)

(defcustom javacomp-log-request-response nil
  "Whether or not to log the requests to and responses from JavaComp server.

If it's non-nil, requests and responses are logged to a buffer
named *javacomp-debug*. Note that `javacomp-log-debug-message'
doesn't affect whether the requests and responses are logged.

This option is for debugging purposes."
  :type 'boolean
  :group 'javacomp)

(defcustom javacomp-log-debug-message nil
  "Whether or not to log messages for debugging.

If it's non-nil, requests and responses are logged to a buffer named *javacomp-debug*."
  :type 'boolean
  :group 'javacomp)

(defmacro javacomp-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable with initial value INIT-VALUE."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar javacomp-request-counter 0)
(defvar javacomp--server-id 0)

(javacomp-def-permanent-buffer-local javacomp-project-root nil)
(javacomp-def-permanent-buffer-local javacomp-buffer-dirty nil)
(javacomp-def-permanent-buffer-local javacomp-buffer-tmp-file nil)
(javacomp-def-permanent-buffer-local javacomp--buffer-version 0)
(javacomp-def-permanent-buffer-local javacomp--buffer-server-id nil)

(defvar javacomp-server-buffer-name "*javacomp-server*")
(defvar javacomp-requestcounter 0)
(defvar javacomp-servers (make-hash-table :test 'equal))
(defvar javacomp-response-callbacks (make-hash-table :test 'equal))
(defvar javacomp-notification-listeners (make-hash-table :test 'equal))

(defface javacomp-file
  '((t (:inherit dired-header)))
  "Face for file names in references output."
  :group 'javacomp)

(defface javacomp-line-number
  '((t (:inherit compilation-line-number)))
  "Face for line numbers in references output."
  :group 'javacomp)

(defface javacomp-match
  '((t (:inherit match)))
  "Face for matched symbol in references output."
  :group 'javacomp)

(defun javacomp-project-root ()
  "Determine project root."
  (or
   javacomp-project-root
   (let* ((projectile-require-project-root nil)
          (root (or (projectile-project-root)))
          (full-path (expand-file-name root)))
     (setq javacomp-project-root full-path)
     full-path)))

(defun javacomp-project-name (&optional project-root)
  "Determine the name of the current project based on the PROJECT-ROOT.

If PROJECT-ROOT is not specified, use the project root returned from `javacomp-project-root'."
  (javacomp--log-debug "project-root: %s" project-root)
  (file-name-nondirectory
   (directory-file-name (or project-root (javacomp-project-root)))))

(defun javacomp-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (javacomp-decode-response process))

(defun javacomp-net-sentinel (process message)
  "Handle signals of the JavaComp process.

PROCESS is the JavaComp process. MESSAGE is the process message.
See `set-process-sentinel' for the two parameters."
  (let ((project-root (javacomp--server-project-root process)))
    (message "(%s) JavaComp server exits: %s." (javacomp-project-name project-root) (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (javacomp-cleanup-project project-root)))

(defun javacomp-cleanup-buffer-callbacks ()
  "Remove all callbacks for the current buffer."
  (let ((error-response `(:errors "Cleaning up buffer callbacks")))
    (maphash
     (lambda (id callback)
       (when (equal (current-buffer) (car callback))
         (funcall (cdr callback) error-response)
         (remhash id javacomp-response-callbacks)))
     javacomp-response-callbacks)))

(defun javacomp-cleanup-project (project-root)
  "Shutdown the server for PROJECT-ROOT and clean up buffers attached to it."
  (javacomp-each-server-buffer (javacomp-project-server project-root)
                        (lambda (buffer)
                          (with-current-buffer buffer
                            (javacomp-cleanup-buffer-callbacks))))
  (remhash project-root javacomp-servers)
  ;; (remhash project-root javacomp-tsserver-unsupported-commands)
  ;; (remhash project-root javacomp-project-configs)
  )

(defun javacomp-decode-response-legth ()
  "Return response length in bytes from the Content-Length header."
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun javacomp-enough-response-p (length)
  "Determine whether the received response has length no less than LENGTH."
  (save-excursion
    (when (search-forward "{" nil t)
      (backward-char 1)
      (>= (- (position-bytes (point-max)) (position-bytes (point))) (1- length)))))

(defun javacomp-decode-response (process)
  "Decode a response returned from PROCESS."
  (with-current-buffer (process-buffer process)
    (let ((length (javacomp-decode-response-legth))
          (json-object-type 'plist)
          (json-array-type 'list))
      (when (and length (javacomp-enough-response-p length))
        (javacomp-dispatch
         (prog2
             (progn
               (search-forward "{")
               (backward-char 1))
             (json-read-object)
           (delete-region (point-min) (point))))
        (when (>= (buffer-size) 16)
          (javacomp-decode-response process))))))

(defun javacomp-dispatch-response (response)
  "Dispatch RESPONSE of a command to its callback function."
  (let* ((request-id (plist-get response :id))
         (callback (gethash request-id javacomp-response-callbacks)))
    (when callback
      (with-current-buffer (car callback)
        (apply (cdr callback) (list response)))
      (remhash request-id javacomp-response-callbacks))))

(defun javacomp-dispatch-notification (notification)
  (-when-let (listener (gethash (javacomp-project-name) javacomp-notification-listeners))
    (with-current-buffer (car listener)
      (apply (cdr listener) (list event)))))

(defun javacomp-dispatch (response)
  "Dispatch a RESPONSE.

If RESPONSE has a field named 'id', dispatch it using
`javacomp-dispatch-response', otherwise dispatch it using
`javacomp-dispatch-notification'."
  (when javacomp-log-request-response
    (javacomp--log-debug-no-check "=========== response =============\n%s" response))
  (if (plist-get response :id)
      (javacomp-dispatch-response response)
    (javacomp-dispatch-notification response)))

(defun javacomp-send-request
    (name args &optional callback skip-initialization-check skip-buffer-sync)
  "Send a request of NAME to JavaComp server.

ARGS is the value of the 'params' fields of the request body.

CALLBACK is a function to be called when the response for the
request is received.

If SKIP-INITIALIZATION-CHECK is non-nil, send the request
regardless the initialization status of the JavaComp server.
Otherwise only send the request if the server is initialized.

If SKIP-BUFFER-SYNC is nil, sync the buffer contents before
sending the request."
  (let ((server (javacomp-current-server)))
    (unless server
      (error "Server does not exist. Run M-x javacomp-restart-server to start it again"))

    (when (or skip-initialization-check (javacomp--server-initialized-p server))
      (unless skip-buffer-sync
        (javacomp-sync-buffer-contents))

      (let* ((request-id (javacomp-next-request-id))
             (request `(:method ,name :id ,request-id :params ,args)))
        (javacomp--send-message request)
        (when callback
          (puthash request-id (cons (current-buffer) callback) javacomp-response-callbacks)
          (accept-process-output nil 0.01))))))

(defun javacomp-send-notification (name args &optional skip-initialization-check)
  "Send a notification to JavaComp server.

A notification is a request without the 'id' field.

NAME is the name of the notification. ARGS is the value of the
'params' field of the request.

If SKIP-INITIALIZATION-CHECK is non-nil, send the notification
regardless whether the JavaComp server is initialized or not.
Otherwise only send the notification if the JavaComp is
initialized."
  (let ((server (javacomp-current-server)))
    (unless server
      (error "Server does not exist. Run M-x javacomp-restart-server to start it again"))
    (when (or skip-initialization-check
              (javacomp--server-initialized-p server))
      (javacomp--send-message `(:method ,name :params ,args)))))

(defun javacomp--send-message (json-content)
  "Encode and send JSON-CONTENT to JavaComp server.

JSON-CONTENT will be encoded to a JSON string."
  (let* ((json-encoding-pretty-print nil)
         (content (json-encode json-content))
         (content-length (number-to-string (string-bytes content)))
         (message (concat
                   "Content-Length: " content-length "\r\n"
                   "Content-Type: application/javacomp-el;charset=utf8\r\n"
                   "\r\n"
                   content)))
    (when javacomp-log-request-response
      (javacomp--log-debug-no-check "~~~~~~~~~~~~~ request ~~~~~~~~~~~~~\n%s" message))
    (process-send-string (javacomp-current-server) message)))

(defun javacomp-send-request-sync (name args)
  "Send a request to JavaComp synchronousely.

NAME is the name of the request. ARGS is the value of the
'params' field of the request.

The timeout of the request is `javacomp-sync-request-timeout'."
  (let* ((start-time (current-time))
         (response nil))
    (javacomp-send-request name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (cadr (time-subtract (current-time) start-time))
               javacomp-sync-request-timeout)
        (error "Sync request timed out %s" name)))
    response))

(defun javacomp-current-server ()
  "Return the JavaComp server for the project root of the current buffer."
  (javacomp-project-server (javacomp-project-root)))

(defun javacomp-project-server (project-root)
  "Return the JavaComp server for PROJECT-ROOT."
  (gethash project-root javacomp-servers))

(defun javacomp-next-request-id ()
  "Return the ID of the next request."
  (number-to-string (cl-incf javacomp-request-counter)))

(defun javacomp-start-server ()
  "Start JavaComp server."
  (when (javacomp-current-server)
    (error "Server already exist"))

  (message "(%s) Starting JavaComp server..." (javacomp-project-name))
  (let* ((default-directory (javacomp-project-root))
         (buf (generate-new-buffer javacomp-server-buffer-name))
         (jar (and javacomp-server-jar
                          (expand-file-name javacomp-server-jar)))
         ; Use pipe for process connection, because pty might transform \r to \n.
         (process-connection-type nil)
         (commands `(,javacomp-java-executable ,@javacomp-java-options "-jar" ,jar))
         (process
          (apply #'start-file-process "javacomp" buf commands)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'javacomp-net-filter)
    (set-process-sentinel process #'javacomp-net-sentinel)
    (set-process-query-on-exit-flag process nil)
    (process-put process 'project-root (javacomp-project-root))
    (process-put process 'server-id (cl-incf javacomp--server-id))
    (javacomp--server-set-initialized process nil)
    (puthash (javacomp-project-root) process javacomp-servers)
    (message "(%s) JavaComp server started successfully." (javacomp-project-name))
    (javacomp-command:initialize)))

(defun javacomp-start-server-if-required ()
  "Start JavaComp server if it's not started."
  (when (not (javacomp-current-server))
    (javacomp-start-server)))

(defun javacomp--server-buffer-list (server)
  "Get a list of buffers with javacomp-mode enabled and connected to SERVER."
  (when server
    (javacomp--server-get server 'javacomp-buffers)))

(defun javacomp--server-add-buffer (server buffer)
  "Add BUFFER to SERVER.

The added buffer will be added to the list returned by `javacomp--server-buffer-list'"
  (let ((buffers (and server (javacomp--server-buffer-list server))))
    (javacomp--server-put server 'javacomp-buffers (cons buffer buffers))))

(defun javacomp--server-remove-buffer (server buffer)
  "Remove BUFFER from SERVER.

Return the buffer list after removal."
  (let* ((buffers (javacomp--server-buffer-list server))
         (remain-buffers (and buffers (delq buffer buffers))))
    (javacomp--server-put server 'javacomp-buffers remain-buffers)
    remain-buffers))

(defun javacomp-each-server-buffer (server fn)
  "Callback FN for each buffer javacomp-mode enabled and attached to SERVER."
  (-each (javacomp--server-buffer-list server) fn))

(defun javacomp-each-project-buffer (project-root fn)
  "Callback FN for each buffer within PROJECT-ROOT with javacomp-mode enabled.

FN is called with no argument and the current buffer set to the matched buffer."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p javacomp-mode)
                   (equal (javacomp-project-root) project-root))
          (funcall fn))))))

(defun javacomp--setup-buffer ()
  "Attach the current buffer to the JavaComp server.

If the server is initialized, send textDocument/didOpen command
to it. Otherwise queue the command until it's initialized.

The current buffer will be added to the server's buffer list."
  (-when-let (server (javacomp-current-server))
    (when (javacomp--server-initialized-p server)
      (javacomp-command:did-open-text-document)
      (javacomp--server-add-buffer server (current-buffer)))))

(defun javacomp--cleanup-buffer ()
  "Detach the current buffer from the JavaComp server.

If the server is initialized, send textDocument/didClose command to it.

The current buffer will be removed from the server's buffer list.
If the current buffer is the last buffer associated with the
server, shutdown the server."
  (let ((server (javacomp-current-server)))
    (when (javacomp--server-initialized-p server)
      (javacomp-command:did-close-text-document))
    (javacomp-cleanup-buffer-callbacks)
    (unless (javacomp--server-remove-buffer server (current-buffer))
      ;; No buffer is using the server, shut it down.
      (javacomp-shutdown-current-project))))

(defun javacomp-handle-change (_beg _end _len)
  "Mark buffer as dirty."
  (setq javacomp-buffer-dirty t))

(defun javacomp--uri-to-filename (uri)
  "Return the file name from URI."
  (cond ((string-prefix-p "/" uri)
         uri)
        ((string-prefix-p "file://" uri)
         (substring uri (length "file://")))
        (t
         (error "Unknown URI scheme: %s" uri))))

(defun javacomp--buffer-uri ()
  "Return the URI for the current buffer."
  (and buffer-file-name (concat "file://" buffer-file-name)))

(defun javacomp--buffer-identifier ()
  "Get the TextDocumentIdentifier JSON message for current buffer."
  `(:uri ,(javacomp--buffer-uri)))

(defun javacomp--buffer-versioned-identifier ()
  "Get the VersionedTextDocumentIdentifier JSON message for current buffer."
  (let ((uri (javacomp--buffer-uri))
        (new-version (cl-incf javacomp-request-counter)))
    `(:uri ,uri :version ,new-version)))

(defun javacomp--buffer-content ()
  "Get the whole buffer content regardless it's narrowed or not."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun javacomp--text-document-position ()
  "Get the TextDocumentPositionParams message from current buffer and point."
  (list :textDocument (javacomp--buffer-identifier)
        :position (javacomp--buffer-current-position)))

(defun javacomp--line-number-at-pos (&optional pos)
  "The line number where the cursor is at.

The position of the cursor is specified by POS.  If POS is nil, use the current
cursor position.

Line number is 0-based."
  (let ((p (or pos (point))))
    (if (= (point-min) 0)
        (1- (line-number-at-pos p))
      (save-excursion
        (save-restriction
          (widen)
          (1- (line-number-at-pos p) ))))))

(defun javacomp--character-offset-in-line ()
  "Number of characters present from the begining of line to cursor in current line.

Offset is 0-based."
  (- (point) (line-beginning-position)))

(defun javacomp--buffer-current-position ()
  "Get a Position message for the cursor position in the current buffer."
  `(:line ,(javacomp--line-number-at-pos) :character ,(javacomp--character-offset-in-line)))

(defun javacomp--language-id ()
  "Get the language ID for the current buffer."
  "java")

(defun javacomp-sync-buffer-contents ()
  "Send buffer content to the server if it has been changed since last sync."
  (when (and (javacomp-current-server) javacomp-buffer-dirty)
    (setq javacomp-buffer-dirty nil)
    (if (not (javacomp--document-opened))
        (javacomp-command:did-open-text-document)
      (let* ((content (javacomp--buffer-content))
             (content-change `(:text ,content))
             (text-document (javacomp--buffer-versioned-identifier)))
        (javacomp-send-notification "textDocument/didChange"
                                    `(:textDocument ,text-document :contentChanges [,content-change]))))))

(defun javacomp--server-set-initialized (server initialized)
  "Set the initialized state of SERVER to INITIALIZED."
  (javacomp--server-put server 'initialized initialized))

(defun javacomp--server-initialized-p (server)
  "Return whether SERVER is initialized.

The SERVER is initialized if it responses to the initialize
command, and no shutdown command is sent after that. If SERVER is
not initialized, only initialize and shutdown commands are
allowed."
  (javacomp--server-get server 'initialized))


;;; Helpers

(defun javacomp-response-success-p (response)
  "Determine whether RESPONSE is a successful response."
  (and response (not (plist-get response :error))))

(defmacro javacomp-on-response-success (response &rest body)
  "If RESPONSE is a successful response, execute BODY."
  (declare (indent 1))
  `(if (javacomp-response-success-p ,response)
       (progn ,@body)
     (-when-let (err (plist-get response :error))
       (javacomp--log-debug "response error: %s" err))
     nil))

(defmacro javacomp-on-response-success-callback (response &rest body)
  "If RESPONSE is a successful response, run BODY."
  (declare (indent 1))
  `(lambda (,response)
     (javacomp-on-response-success ,response
       ,@body)))

(defun javacomp--server-get (server propname)
  "Return the value of SERVER's PROPNAME property."
  (when server
    (process-get server propname)))

(defun javacomp--server-put (server propname value)
  "Change SERVER's PROPNAME property to VALUE."
  (when server
    (process-put server propname value)))

(defun javacomp--current-server-get (propname)
  "Return the value of current server's PROPNAME property."
  (javacomp--server-get (javacomp-current-server) propname))

(defun javacomp--current-server-put (propname value)
  "Change current server's PROPNAME property to VALUE."
  (javacomp--server-put (javacomp-current-server) propname value))

(defun javacomp--current-server-id ()
  "Return the ID of the current server, or nil if server is not started."
  (javacomp--current-server-get 'server-id))

(defun javacomp--server-project-root (server)
  "Return the project root associated with SERVER."
  (javacomp--server-get server 'project-root))

(defun javacomp--document-opened ()
  "Determine whether textDocument/didOpen notification is sent to the current server."
  (eq javacomp--buffer-server-id (javacomp--current-server-id)))

(defun javacomp--lines-text (filename line-numbers)
  "Get the content of FILENAME at the lines of LINE-NUMBERS.

Return an alist of (line-number . line-text)."
  (when line-numbers
      (with-temp-buffer
        (insert-file-contents filename)
        (mapcar (lambda (line-number)
                  (let (start-point
                        end-point)
                    (goto-char (point-min))
                    (forward-line line-number)
                    (setq start-point (point))
                    (move-end-of-line nil)
                    (setq end-point (point))
                    (cons line-number (buffer-substring start-point end-point))))
                line-numbers))))

(defun javacomp--line-text (filename line-number)
  "Get the content of FILENAME at the line of LINE-NUMBER."
  (cdar (javacomp--lines-text filename (list line-number))))

(defun javacomp-plist-get (list &rest args)
  (cl-reduce
   (lambda (object key)
     (when object
       (plist-get object key)))
   args
   :initial-value list))

(defun javacomp--log-debug (format-string &rest args)
  "Log a debug message.

Append message to *javacomp-debug* buffer if
`javacomp-log-debug-message' is non-nil.

FORMAT-STRING is a template for formatting ARGS. See
`format-message' for more information."
  (when javacomp-log-debug-message
    (apply #'javacomp--log-debug-no-check format-string args)))

(defun javacomp--log-debug-no-check (format-string &rest args)
  "Log a debug message regardless the value of `javacomp-log-debug-message'.

The debug message is logged to *javacomp-debug* buffer.

FORMAT-STRING is a template for formatting ARGS. See
`format-message' for more information."
  (with-current-buffer (get-buffer-create "*javacomp-debug*")
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (let ((str (apply #'format-message format-string args)))
          (insert (substring str 0 (min (length str) 512))
                  "\n\n"))))))

(defun javacomp--safe-fontify-string (java-snippet)
  "Syntax highlight JAVA-SNIPPET.

If JAVA-SNIPPET is a string, return a syntax highlighted string
with the same content of JAVA-SNIPPET. Otherwise return
JAVA-SNIPPET itself. In either case, JAVA-SNIPPET is unchanged."
  (if (stringp java-snippet)
    (with-temp-buffer
      (let ((java-mode-hook nil)
            (c-mode-common-hook nil)
            (prog-mode-hook)
            (inhibit-message t))
        (insert java-snippet)
        (java-mode)
        (font-lock-fontify-buffer)
        (buffer-string)))
    java-snippet))

;;; Jumping

(defvar javacomp-references-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'javacomp-find-next-location)
    (define-key map (kbd "p") #'javacomp-find-previous-location)
    (define-key map (kbd "C-m") #'javacomp-goto-location)
    (define-key map [mouse-1] #'javacomp-goto-location)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode javacomp-references-mode nil "javacomp-references"
  "Major mode for javacomp references list.

\\{javacomp-references-mode-map}"
  (use-local-map javacomp-references-mode-map)
  (setq buffer-read-only t)
  (setq next-error-function #'javacomp-next-location-function))

(defun javacomp--list-locations (locations)
  "Show a buffer with a list of file locations from LOCATIONS."
  (pop-to-buffer (javacomp--locations-buffer locations)))

(defun javacomp--locations-buffer (locations)
  "Create a buffer with the given LOCATIONS."
  (let ((buffer (get-buffer-create "*javacomp-references*"))
        (inhibit-read-only t)
        (width tab-width)
        (project-root (javacomp-project-root))
        (last-file-name nil))
    (with-current-buffer buffer
      (erase-buffer)
      (javacomp-references-mode)
      (setq tab-width width)
      (while locations
        (let* ((location (car locations))
               (full-file-name (javacomp--uri-to-filename (plist-get location :uri)))
               (file-name (file-relative-name full-file-name project-root))
               (line-number (javacomp-plist-get location :range :start :line))
               (line-text (javacomp--line-text full-file-name line-number)))

          ;; file
          (when (not (equal last-file-name file-name))
            (setq last-file-name file-name)
            (insert (propertize file-name 'face 'javacomp-file))
            (insert "\n"))

          (insert
           (javacomp-concat-and-annotate-line location
                                              (concat
                                               ;; line number
                                               (propertize (format "%5d" line-number) 'face 'javacomp-line-number)
                                               ":")
                                               ;; line text
                                              line-text))


          (insert "\n"))
        (pop locations))
      (goto-char (point-min))
      (current-buffer))))

(defun javacomp-concat-and-annotate-line (location line-prefix line-text)
  "Concat LINE-PREFIX and LINE-TEXT and set text properties.

LOCATION is a Location JSON message defined by Language Server
Protocol."
  (let ((start (javacomp-plist-get location :range :start :character))
        (end (javacomp-plist-get location :range :end :character)))
    (put-text-property start end 'face 'javacomp-match line-text))
  (let* ((line (concat line-prefix line-text))
         (len (length line)))
    (put-text-property 0 len 'mouse-face 'highlight line)
    (put-text-property 0 len 'help-echo "mouse-1: Visit the location." line)
    (put-text-property 0 len 'javacomp-location location line)
    line))

(defun javacomp--move-to-position (position)
  "Move point to POSITION in the current buffer.

POSITION is the JSON Position message defined by Language Server Protocol."
  (let* ((line (plist-get position :line))
         (character (plist-get position :character)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line line))
    (forward-char character)
    (recenter)))

(defun javacomp--position-to-point (position)
  "Convert POSITION to point of the buffer.

POSITION is the JSON Position message defined by Language Server Protocol."
  (save-excursion
    (javacomp--move-to-position position)
    (point)))

(defun javacomp--jump-to-location (location &optional reuse-window no-marker)
  "Jump to the file and point specified by LOCATION.

LOCATION is the JSON Location message defined by Language Server Protocol.

If RESUE-WINDOW is non-nil, show the buffer of the file of the
location in the current window. Otherwise, show the buffer in a
new window if possible."
  (let* ((uri (plist-get location :uri))
         (file (javacomp--uri-to-filename uri)))
    (unless no-marker
      (ring-insert find-tag-marker-ring (point-marker)))
    (if reuse-window
        (pop-to-buffer (find-file-noselect file)
                       '((display-buffer-reuse-window display-buffer-same-window)))
      (pop-to-buffer (find-file-noselect file)))
    (javacomp--move-to-position (plist-get (plist-get location :range) :start))))

(defun javacomp-next-location-function (n &optional reset)
  "Override for `next-error-function' for use in javacomp-reference-mode buffers."
  (interactive "p")

  (-when-let (buffer (get-buffer "*javacomp-references*"))
    (with-current-buffer buffer
      (when reset
        (goto-char (point-min)))
      (if (> n 0)
          (javacomp-find-next-location (point) n)
        (javacomp-find-previous-location (point) (- n)))
      (javacomp-goto-location))))

(defun javacomp-find-next-location (pos arg)
  "Move to next location."
  (interactive "d\np")
  (setq arg (* 2 arg))
  (unless (get-text-property pos 'javacomp-location)
    (setq arg (1- arg)))
  (dotimes (_i arg)
    (setq pos (next-single-property-change pos 'javacomp-location))
    (unless pos
      (error "Moved past last location")))
  (goto-char pos))

(defun javacomp-find-previous-location (pos arg)
  "Move back to previous location."
  (interactive "d\np")
  (dotimes (_i (* 2 arg))
    (setq pos (previous-single-property-change pos 'javacomp-location))
    (unless pos
      (error "Moved back before first location")))
  (goto-char pos))

(defun javacomp-goto-location ()
  "Jump to reference location in the file."
  (interactive)
  (-when-let (location (get-text-property (point) 'javacomp-location))
    (javacomp--jump-to-location location nil t)))

;;; Requests

(defun javacomp--initialization-options ()
  "Return the options to be sent with the initialize command."
  (let ((options nil))
    (when javacomp-options-log-level
      (setq options (plist-put options :logLevel javacomp-options-log-level)))
    (when (and javacomp-options-log-path
               (> (length javacomp-options-log-path) 0))
      (setq options (plist-put options :logPath javacomp-options-log-path)))
    (when javacomp-options-ignore-paths
      (setq options (plist-put options :ignorePaths javacomp-options-ignore-paths)))
    (when javacomp-options-type-index-files
      (setq options (plist-put options :typeIndexFiles javacomp-options-type-index-files)))
    options))

(defun javacomp-command:initialize ()
  "Send `initialize' request to JavaComp server."
  (let* ((server (javacomp-current-server))
         (callback (lambda (response)
                     (javacomp--on-initialize-response server response)))
         (root-uri (concat "file://" (javacomp-project-root)))
         (options (javacomp--initialization-options)))
    (javacomp-send-request "initialize"
                           `(:processId ,(emacs-pid) :rootUri ,root-uri :initializationOptions ,options)
                           callback
                           t
                           t)))

(defun javacomp--on-initialize-response (server response)
  "Callback function for initialize command.

SERVER is the JavaComp server. RESPONSE is the response for
initialize command sent from the SERVER.

Set the initialized status of SERVER to t and set up the buffers
under the project root of SERVER."
  (javacomp-on-response-success response
    (javacomp--server-set-initialized server t)
    (javacomp-each-project-buffer (javacomp--server-project-root server)
                                  #'javacomp--setup-buffer)))

(defun javacomp-command:shutdown ()
  "Send `shutdown' request to JavaComp server."
  (let* ((server (javacomp-current-server))
         (callback (lambda (_resp)
                    (javacomp--server-set-initialized server nil))))
    ;; Shutdown may be sent when the last buffer associated with the server is
    ;; being kill. In this case, when the response is received, the buffer is no
    ;; longer alive. Dispatching a response to callback associated with a killed
    ;; buffer will cause error. To avoid that, use the server's buffer.
    (with-current-buffer (process-buffer server)
      (javacomp-send-request "shutdown" nil callback t t))))

(defun javacomp-command:did-open-text-document ()
  "Send `textDocument/didOpen' notification to JavaComp server."
  (let* ((text-document `(:uri ,(javacomp--buffer-uri)
                               :languageId ,(javacomp--language-id)
                               :version ,javacomp--buffer-version
                               :text ,(javacomp--buffer-content))))
    (javacomp-send-notification "textDocument/didOpen"
                                `(:textDocument ,text-document))
    (setq javacomp--buffer-server-id (javacomp--current-server-id))))

(defun javacomp-command:did-close-text-document ()
  "Send `textDocument/didClose' notification to JavaComp server."
  (when (javacomp--document-opened)
    (setq javacomp--buffer-server-id nil)
    (javacomp-send-notification "textDocument/didClose"
                                `(:textDocument ,(javacomp--buffer-identifier)))))

(defun javacomp-notification:exit ()
  "Send `exit' notification to JavaComp server."
  (javacomp-send-notification "exit" nil t))

;;; eldoc

(defun javacomp-method-call-p ()
  "Determine whether the cursor is in a method call."
  (or (looking-at "[(,]") (and (not (looking-at "\\sw")) (looking-back "[(,]\n?\\s-*" nil))))

(defun javacomp-eldoc-maybe-show (text)
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
         (eldoc-message text))))

(defun javacomp-hover-text (response)
  "Extract text from Hover response."
  (let* ((content (javacomp-plist-get response :result :contents))
         ; content can be a single MarkedString or a list. Normalize single MarkedString to
         ; a list
         (contents (if (or (stringp content) (plist-get response :value))
                       (list content)
                     content)))
    (when content
      (mapconcat #'javacomp-marked-string-to-text contents "\n"))))

(defun javacomp-marked-string-to-text (marked)
  "Convert a MarkedString MARKED to a string."
  (if (stringp marked)
      ; TODO: convert markdown string to formatted string.
      marked
    ; TODO: format language string by language
    (plist-get marked :value)))

(defun javacomp-signature-text (response)
  "Extract text from SignatureHelp RESPONSE."
  (let ((signatures (javacomp-plist-get response :result :signatures)))
    (javacomp--log-debug "signature %s %s" response signatures)
    (when signatures
      ; TODO: format the signature based on parameters.
      (plist-get (car signatures) :label))))

(defun javacomp-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (if (javacomp-method-call-p)
        (javacomp-send-request
         "textDocument/signatureHelp"
         (javacomp--text-document-position)
         (javacomp-on-response-success-callback response
           (javacomp-eldoc-maybe-show (javacomp--safe-fontify-string
                                       (javacomp-signature-text response)))))
      (when (looking-at "\\s_\\|\\sw")
        (javacomp-send-request
         "textDocument/hover"
         (javacomp--text-document-position)
         (javacomp-on-response-success-callback response
           (javacomp-eldoc-maybe-show (javacomp--safe-fontify-string
                                       (javacomp-hover-text response))))))))
  nil)

;;; Auto completion

(defconst javacomp--completion-kinds-annotation
  ;; See CompletionItemKind enum defined in
  ;; https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textDocument_completion
  '(nil
    nil ;; Text
    " method" ;; Method
    " function" ;; Function
    " constructor" ;; Constructor
    " field" ;; Field
    " variable" ;; Variable
    " class" ;; Class
    " interface" ;; Interface
    " package" ;; Module
    " property" ;; Property
    " unit" ;; Unit
    " value" ;; Value
    " enum" ;; Enum
    " keyword" ;; Keyword
    " snippet" ;; Snippet
    " color" ;; Color
    " file" ;; File
    " reference" ;; Reference
    )
  "A list mapping CompletionItemKind enum values to completion annotation strings.")

(defun javacomp-completion-annotation (name)
  "Return a short string of the type of the completion item NAME."
  (let* ((item (get-text-property 0 'completion-item name))
         (item-detail (plist-get item :detail))
         (item-kind (plist-get item :kind)))
    (if item-detail
        ;; Get everything before the first newline, if any, because company-mode
        ;; wants single-line annotations.
        (concat " " (car (split-string item-detail "\n")))
      (nth item-kind javacomp--completion-kinds-annotation))))

(defun javacomp-completion-prefix ()
  "Return the completion prefix for the current point for company."
  ; TODO: Use server provided triggers.
  (let ((symbol-cons (company-grab-symbol-cons "\\." 1)))
    (if (and (stringp symbol-cons) (string-prefix-p "@" symbol-cons))
        ;; @ is part of the symbol. Remove it from the prefix and always
        ;; show completion for annotations.
        (cons (substring symbol-cons 1) t)
      symbol-cons)))

(defun javacomp-annotate-completions (completion-list prefix text-document-position)
  (-map
   (lambda (completion-item)
     (let ((label (plist-get completion-item :label)))
       (put-text-property 0 1 'text-document-position text-document-position label)
       (put-text-property 0 1 'completion-item completion-item label)
       label))
    (plist-get completion-list :items)))

(defun javacomp-command:completion-text-document (prefix cb)
  "Send textDocument/completion command to JavaComp server.

PREFIX is the prefix provided by company-mode. CB will be called
on the response from JavaComp server."
  (let ((text-document-position (javacomp--text-document-position)))
    (javacomp-send-request "textDocument/completion"
                           text-document-position
                           (lambda (response)
                             (funcall
                              cb
                              (when (javacomp-response-success-p response)
                                (javacomp-annotate-completions (plist-get response :result) prefix text-document-position)))))))

;;;###autoload
(defun company-javacomp (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-javacomp))
    (prefix (and
             (bound-and-true-p javacomp-mode)
             ;; (-any-p #'derived-mode-p javacomp-supported-modes)
             (javacomp-current-server)
             (not (company-in-string-or-comment))
             (or (javacomp-completion-prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb)
                        (javacomp-command:completion-text-document arg cb))))
    (sorted t)
    (ignore-case t)
    (no-cache t)
    ;; (meta (javacomp-completion-meta arg))
    (annotation (javacomp-completion-annotation arg))
    ;; (doc-buffer (javacomp-completion-doc-buffer arg))
    ))

(eval-after-load 'company
  '(progn
     (cl-pushnew 'company-javacomp company-backends)))

;;; Mode

(defvar javacomp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "M-.") #'javacomp-jump-to-definition)
    ;; (define-key map (kbd "M-,") #'javacomp-jump-back)
    map))

(defun javacomp--enable ()
  "Setup `javacomp-mode' in current buffer."
  (javacomp-start-server-if-required)
  (javacomp--setup-buffer)
  (set (make-local-variable 'eldoc-documentation-function)
       'javacomp-eldoc-function)
  ;; (set (make-local-variable 'imenu-auto-rescan) t)
  ;; (set (make-local-variable 'imenu-create-index-function)
  ;;      'javacomp-imenu-index)

  (add-hook 'after-save-hook 'javacomp-sync-buffer-contents nil t)
  ;; (add-hook 'after-save-hook 'javacomp-auto-compile-file nil t)
  (add-hook 'after-change-functions 'javacomp-handle-change nil t)
  (add-hook 'kill-buffer-hook 'javacomp--cleanup-buffer nil t))

(defun javacomp--disable ()
  "Disable `javacomp-mode' in current buffer and clean up."
  (remove-hook 'after-save-hook 'javacomp-sync-buffer-contents)
  ;; (remove-hook 'after-save-hook 'javacomp-auto-compile-file)
  (remove-hook 'after-change-functions 'javacomp-handle-change)
  (remove-hook 'kill-buffer-hook 'javacomp--cleanup-buffer)
  (javacomp--cleanup-buffer)
  ;; (set (make-local-variable 'imenu-auto-rescan) t)
  ;; (set (make-local-variable 'imenu-create-index-function)
  ;;      'javacomp-imenu-index)
  )

(defun javacomp-shutdown-current-project ()
  "Shutdown the current project and the JavaComp server if it's running."
  (interactive)
  (when (javacomp-current-server)
    (condition-case err
        (progn
          (javacomp-command:shutdown)
          (javacomp-notification:exit))
      (error
       (javacomp--log-debug (error-message-string err))))
    (javacomp-cleanup-project (javacomp-project-root))))

(defun javacomp-restart-server ()
  "Restarts the JavaComp server for the current project if enabled."
  (interactive)
  (-when-let (server (javacomp-current-server))
    (if (process-live-p server)
        (delete-process server)
      (javacomp--log-debug "JavaComp server died but not cleaned up.")
      (javacomp-cleanup-project (javacomp-project-root))))
  (javacomp-start-server))

(defun javacomp-jump-to-definition ()
  "Jump to the definition of the symbol at point.

If more than one definition is returned, list all definitions in a separate buffer."
  (interactive)
  (let ((cb (lambda (response)
              (javacomp-on-response-success response
                (let* ((result (plist-get response :result))
                       ;; result can be either a single Location, or an array of Locations.
                       (locations (if (plist-get result :uri)
                                      (list result)
                                    result))
                       (len (length locations)))
                  (javacomp--log-debug "definition len: %s, def: %s" len locations)
                  (cond ((eq len 0)
                         (message "No definition found"))
                        ((eq len 1)
                         (javacomp--jump-to-location (car locations) t))
                        (t
                         (javacomp--list-locations locations))))))))
    (javacomp-send-request "textDocument/definition" (javacomp--text-document-position) cb)))

;;;###autoload
(define-minor-mode javacomp-mode
  "Minor mode for JavaComp.

\\{javacomp-mode-map}"
  :lighter " javacomp"
  :keymap javacomp-mode-map
  (if javacomp-mode
      (javacomp--enable)
    (javacomp--disable)))


(provide 'javacomp)

;;; javacomp.el ends here
