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

(defvar javacomp-request-counter 0)

(javacomp-def-permanent-buffer-local javacomp-project-root nil)
(javacomp-def-permanent-buffer-local javacomp-buffer-dirty nil)
(javacomp-def-permanent-buffer-local javacomp-buffer-tmp-file nil)
(javacomp-def-permanent-buffer-local javacomp--buffer-version 0)
(javacomp-def-permanent-buffer-local javacomp--document-opened nil)

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
    (message "(%s) sending message:\n%s" (javacomp-project-name) message)
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
  ;; (javacomp-command:configure)
  )

(defun javacomp-cleanup-buffer ()
  (javacomp-command:did-close-text-document))

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

(defun javacomp--language-id ()
  "Get the language ID for the current buffer."
  "java")

(defun javacomp-sync-buffer-contents ()
  "Send buffer content to the server if it has been changed since last sync."
  (when (and javacomp--document-opened javacomp-buffer-dirty)
    (setq javacomp-buffer-dirty nil)
    (let* ((content (javacomp--buffer-content))
           (content-changes `(:text ,content))
           (text-document (javacomp--buffer-versioned-identifier)))
      (javacomp-send-notification "textDocument/didChange"
                                  `(:textDocument ,text-document :contentChanges ,content-changes)))))

(defun javacomp--set-server-initialized (server initialized)
  "Set the initialized state of SERVER to INITIALIZED."
  (when server
    (process-put server 'initialized initialized)))

(defun javacomp--server-initialized-p (server)
  "Determine whether SERVER is initialized."
  (process-get server 'initialized))


;;; Helpers

(defun javacomp-response-success-p (response)
  (and response (not (plist-get response :error))))

(defmacro javacomp-on-response-success (response &rest body)
  (declare (indent 1))
  `(if (javacomp-response-success-p ,response)
       ,@body
     (-when-let (err (plist-get response :error))
       (message "(JavaComp) response error: %s" err))
     nil))

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
    (setq javacomp--document-opened t)))

(defun javacomp-command:did-close-text-document ()
  "Send `textDocument/didClose' notification to JavaComp server."
  (when javacomp--document-opened
    (setq javacomp--document-opened nil)
    (javacomp-send-notification "textDocument/didClose"
                                `(:textDocument ,(javacomp--buffer-identifier)))))

(defun javacomp-notification:exit ()
  "Send `exit' notification to JavaComp server."
  (javacomp-send-notification "exit" nil))

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
  (javacomp-shutdown-current-project)
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
