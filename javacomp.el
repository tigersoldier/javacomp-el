;;; javacomp.el --- Java completion engine client -*- lexical-binding: t -*-

;; Copyright (C) 2017 Caibin Chen.

;; Author: Caibin Chen <tigersoldi@gmail.com>
;; URL: http://github.com/tigersoldier/javacomp-el
;; Version: 0.0.1
;; Keywords: java
;; Package-Requires: ((dash "2.10.0") (flycheck "27") (cl-lib "0.5"))

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

(defmacro javacomp-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable with initial value INIT-VALUE."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar javacomp-completion-detailed nil
  "Completion dropdown will contain detailed method information if set to non-nil.")

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


(defun javacomp-project-root ()
  "Determine project root."
  (or
   javacomp-project-root
   (let ((root (or (locate-dominating-file default-directory "WORKSPACE"))))
     (unless root
       (message "Couldn't locate project root directory. Assuming '%s' is the root directory." default-directory)
       (setq root default-directory))
     (let ((full-path (expand-file-name root)))
       (setq javacomp-project-root full-path)
       full-path))))

(defun javacomp-project-name (&optional project-root)
  "Determine the name of the current project based on the PROJECT-ROOT.

If PROJECT-ROOT is not specified, use the project root returned from `javacomp-project-root'."
  (message "project-root: %s" project-root)
  (file-name-nondirectory
   (directory-file-name (or project-root (javacomp-project-root)))))

(defun javacomp-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (javacomp-decode-response process))

(defun javacomp-net-sentinel (process message)
  (let ((project-root (process-get process 'project-root)))
    (message "(%s) JavaComp server exits: %s." (javacomp-project-name project-root) (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (javacomp-cleanup-project project-root)))

(defun javacomp-cleanup-buffer-callbacks ()
  (let ((error-response `(:errors "Cleaning up buffer callbacks")))
    (maphash
     (lambda (id callback)
       (when (equal (current-buffer) (car callback))
         (funcall (cdr callback) error-response)
         (remhash id javacomp-response-callbacks)))
     javacomp-response-callbacks)))

(defun javacomp-cleanup-project (project-root)
  (javacomp-each-buffer project-root
                        (lambda ()
                          (javacomp-cleanup-buffer-callbacks)))
  (remhash project-root javacomp-servers)
  ;; (remhash project-root javacomp-tsserver-unsupported-commands)
  ;; (remhash project-root javacomp-project-configs)
  )

(defun javacomp-decode-response-legth ()
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun javacomp-enough-response-p (length)
  (save-excursion
    (when (search-forward "{" nil t)
      (backward-char 1)
      (>= (- (position-bytes (point-max)) (position-bytes (point))) (1- length)))))

(defun javacomp-decode-response (process)
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
  (message "Got response %s" response)
  (if (plist-get response :id)
      (javacomp-dispatch-response response)
    (javacomp-dispatch-notification response)))

(defun javacomp-send-request (name args &optional callback)
  (when (not (javacomp-current-server))
    (error "Server does not exist. Run M-x javacomp-restart-server to start it again"))


  (javacomp-sync-buffer-contents)

  (let* ((request-id (javacomp-next-request-id))
         (request `(:method ,name :id ,request-id :params ,args)))
    (javacomp--send-message request)
    (when callback
      (puthash request-id (cons (current-buffer) callback) javacomp-response-callbacks)
      (accept-process-output nil 0.01))))

(defun javacomp-send-notification (name args)
  (when (not (javacomp-current-server))
    (error "Server does not exist. Run M-x javacomp-restart-server to start it again"))
  (javacomp--send-message `(:method ,name :params ,args)))

(defun javacomp--send-message (json-content)
  (let* ((json-encoding-pretty-print nil)
         (content (json-encode json-content))
         (content-length (number-to-string (string-bytes content)))
         (message (concat
                   "Content-Length: " content-length "\r\n"
                   "Content-Type: application/javacomp-el;charset=utf8\r\n"
                   "\r\n"
                   content)))
    ;; (message "(%s) sending message:\n%s" (javacomp-project-name) message)
    (process-send-string (javacomp-current-server) message)))

(defun javacomp-send-request-sync (name args)
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
  (gethash (javacomp-project-root) javacomp-servers))

(defun javacomp-next-request-id ()
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
         (process
          (start-file-process "javacomp" buf javacomp-java-executable "-jar" jar)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'javacomp-net-filter)
    (set-process-sentinel process #'javacomp-net-sentinel)
    (set-process-query-on-exit-flag process nil)
    (process-put process 'project-root (javacomp-project-root))
    (process-put process 'server-id (cl-incf javacomp--server-id))
    (javacomp--set-server-initialized process nil)
    (puthash (javacomp-project-root) process javacomp-servers)
    (message "(%s) JavaComp server started successfully." (javacomp-project-name))
    (javacomp-command:initialize)))

(defun javacomp-start-server-if-required ()
  "Start JavaComp server if it's not started."
  (when (not (javacomp-current-server))
    (javacomp-start-server)))

(defun javacomp-each-buffer (project-root fn)
  "Callback FN for each buffer within PROJECT-ROOT with javacomp-mode enabled."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p javacomp-mode)
                   (equal (javacomp-project-root) project-root))
          (funcall fn))))))

(defun javacomp-configure-buffer ()
  (javacomp-command:did-open-text-document)
  (let ((buffers (javacomp--server-get 'javacomp-buffers)))
    (javacomp--server-put 'javacomp-buffers (cons (current-buffer) buffers)))
  ;; (javacomp-command:configure)
  )

(defun javacomp-cleanup-buffer ()
  (javacomp-command:did-close-text-document)
  (let* ((buffers (javacomp--server-get 'javacomp-buffers))
         (remain-buffers (delq (current-buffer) buffers)))
    (if remain-buffers
        (javacomp--server-put 'javacomp-buffers remain-buffers)
      (javacomp-shutdown-current-project))))

(defun javacomp-handle-change (_beg _end _len)
  "Mark buffer as dirty."
  (setq javacomp-buffer-dirty t))

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

(defun javacomp--line-number-at-pos (&optional pos)
  "The line number where the cursor is at.

The position of the cursor is specified by POS.  If POS is nil, use the current
cursor position.

Line number is 1-based."
  (let ((p (or pos (point))))
    (if (= (point-min) 1)
        (line-number-at-pos p)
      (save-excursion
        (save-restriction
          (widen)
          (line-number-at-pos p))))))

(defun javacomp--character-offset-in-line ()
  "Number of characters present from the begining of line to cursor in current line.

offset is 1-based."
  (1+ (- (point) (line-beginning-position))))

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

(defun javacomp--set-server-initialized (server initialized)
  "Set the initialized state of SERVER to INITIALIZED."
  (when server
    (process-put server 'initialized initialized)))

(defun javacomp--server-initialized-p (server)
  "Determine whether SERVER is initialized."
  (process-get server 'initialized))


;;; Helpers

(defun javacomp-response-success-p (response)
  "Determine whether RESPONSE is a successful response."
  (and response (not (plist-get response :error))))

(defmacro javacomp-on-response-success (response &rest body)
  "If RESPONSE is a successful response, execute BODY."
  (declare (indent 1))
  `(if (javacomp-response-success-p ,response)
       ,@body
     (-when-let (err (plist-get response :error))
       (message "(JavaComp) response error: %s" err))
     nil))

(defun javacomp--server-get (propname)
  "Return the value of current server's PROPNAME property."
  (-when-let (server (javacomp-current-server))
    (process-get server propname)))

(defun javacomp--server-put (propname value)
  "Change current server's PROPNAME property to VALUE."
  (-when-let (server (javacomp-current-server))
    (process-put server propname value)))

(defun javacomp--current-server-id ()
  "Return the ID of the current server, or nil if server is not started."
  (javacomp--server-get 'server-id))

(defun javacomp--document-opened ()
  "Determine whether textDocument/didOpen notification is sent to the current server."
  (eq javacomp--buffer-server-id (javacomp--current-server-id)))

;;; Requests

(defun javacomp-command:initialize ()
  "Send `initialize' request to JavaComp server."
  (let ((callback (lambda (resp)
                    (javacomp-on-response-success resp
                      (javacomp--set-server-initialized (javacomp-current-server) t))))
        (root-uri (concat "file://" (javacomp-project-root))))
    (javacomp-send-request "initialize"
                           `(:processId ,(emacs-pid) :rootUri ,root-uri)
                           callback)))

(defun javacomp-command:shutdown ()
  "Send `shutdown' request to JavaComp server."
  (let ((callback (lambda (_resp)
                    (javacomp--set-server-initialized (javacomp-current-server) nil))))
    (javacomp-send-request "shutdown" nil callback)))

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
  (javacomp-send-notification "exit" nil))

;;; Auto completion

(defconst javacomp--completion-kinds-annotation
  ;; See CompletionItemKind enum defined in
  ;; https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textDocument_completion
  '(nil
    nil ;; Text
    " m" ;; Method
    " ƒ" ;; Function
    " c" ;; Constructor
    " v" ;; Field
    " v" ;; Variable
    " C" ;; Class
    " I" ;; Interface
    " M" ;; Module
    " p" ;; Property
    " u" ;; Unit
    " v" ;; Value
    " E" ;; Enum
    " k" ;; Keyword
    " s" ;; Snippet
    " c" ;; Color
    " f" ;; File
    " r" ;; Reference
    )
  "A list mapping CompletionItemKind enum values to completion annotation strings.")

(defun javacomp-completion-annotation (name)
  "Return a short string of the type of the completion item NAME."
  (if javacomp-completion-detailed
      ;; Get everything before the first newline, if any, because company-mode
      ;; wants single-line annotations.
      (car (split-string (javacomp-completion-meta name) "\n"))
    (let ((item-kind (plist-get (get-text-property 0 'completion-item name) :kind)))
      (nth item-kind javacomp--completion-kinds-annotation))))

(defun javacomp-completion-prefix ()
  ; TODO: Use server provided triggers.
  (company-grab-symbol-cons "\\." 1))

(defun javacomp-annotate-completions (completion-list prefix text-document-position)
  (-map
   (lambda (completion-item)
     (let ((label (plist-get completion-item :label)))
       (put-text-property 0 1 'text-document-position text-document-position label)
       (put-text-property 0 1 'completion-item completion-item label)
       label))
   (-filter
    (lambda (completion-item)
      (string-prefix-p prefix (plist-get completion-item :label)))
    (plist-get completion-list :items))))

(defun javacomp-command:completion-text-document (prefix cb)
  (let* ((text-document-position
          `(:textDocument ,(javacomp--buffer-identifier) :position ,(javacomp--buffer-current-position))))
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
  (javacomp-configure-buffer)
  ;; (set (make-local-variable 'eldoc-documentation-function)
  ;;      'javacomp-eldoc-function)
  ;; (set (make-local-variable 'imenu-auto-rescan) t)
  ;; (set (make-local-variable 'imenu-create-index-function)
  ;;      'javacomp-imenu-index)

  (add-hook 'after-save-hook 'javacomp-sync-buffer-contents nil t)
  ;; (add-hook 'after-save-hook 'javacomp-auto-compile-file nil t)
  (add-hook 'after-change-functions 'javacomp-handle-change nil t)
  (add-hook 'kill-buffer-hook 'javacomp-cleanup-buffer nil t)
  (add-hook 'hack-local-variables-hook 'javacomp-configure-buffer nil t))

(defun javacomp--disable ()
  "Disable `javacomp-mode' in current buffer and clean up."
  (remove-hook 'after-save-hook 'javacomp-sync-buffer-contents)
  ;; (remove-hook 'after-save-hook 'javacomp-auto-compile-file)
  (remove-hook 'after-change-functions 'javacomp-handle-change)
  (remove-hook 'kill-buffer-hook 'javacomp-cleanup-buffer)
  (remove-hook 'hack-local-variables-hook 'javacomp-configure-buffer)
  (javacomp-cleanup-buffer)
  ;; (set (make-local-variable 'eldoc-documentation-function)
  ;;      'javacomp-eldoc-function)
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
       (message (error-message-string err))))
    (javacomp-cleanup-project (javacomp-project-root))))

(defun javacomp-restart-server ()
  "Restarts the JavaComp server for the current project if enabled."
  (interactive)
  (-when-let (server (javacomp-current-server))
    (delete-process server))
  (javacomp-start-server)
  (javacomp-each-buffer (javacomp-project-root) #'javacomp-configure-buffer))

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
