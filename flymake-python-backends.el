(require 'f)

(defvar-local flymake-python/mypy--proc nil)
(defvar-local flymake-python/flake8--proc nil)
(defvar-local flymake-python/pylint--proc nil)

(defvar-local flymake-python/mypy--temp-files '())

(defvar flymake-python/mypy--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\) \\(\\[.+\\]\\)$")

(defvar flymake-python/flake8--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\) \\(.+\\)$")

(defvar flymake-python/pylint--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.+\\)$")

(defun flymake-python-backends/buffer-temp-file-name (backend-name)
  ""
  (concat
   "flymake-python-"
   backend-name
   (substring (secure-hash 'md5 (buffer-name))
              0 16)))

(defun flymake-python-backends/buffer-to-temp-file (backend-name)
  ""
  (let ((file-name (f-join (temporary-file-directory)
                               (flymake-python-backends/buffer-temp-file-name
                                backend-name)))
        (write-region-inhibit-fsync t)
        (jka-compr-inhibit t))
    (make-directory (file-name-directory file-name) t)
    (write-region nil nil file-name nil 0)
    file-name))


(defmacro flymake-python-backends/run-backend-for-checker
    (backend-name
     executable
     backend-command
     proc-var
     output-parse-fn
     severity)

  (let ((backend-fn-name (intern (format "flymake-python-backends/%s" backend-name)))
        (process-name (format "%s-flymake" backend-name))
        (process-buffer-name (format " *%s-flymake*" backend-name)))
    `(progn
       (unless (executable-find ,executable)
         (error "Cannot find a %s executable" ,executable))

       ;; If a live process launched in an earlier check was found, that process
       ;; is killed.  When that process's sentinel eventually runs, it will
       ;; notice its obsoletion since PROC-VAR is set to a different value.
       (when (process-live-p ,proc-var)
         (kill-process ,proc-var))

       ;; Save the current buffer, the narrowing restriction, remove any
       ;; narrowing restriction.
       (save-restriction
         (widen)
         (let ((buffer-temp-file
                (flymake-python-backends/buffer-to-temp-file ,backend-name)))
           (setq ,proc-var
                 (make-process
                  :name ,process-name
                  :noquery t
                  :connection-type 'pipe
                  :buffer (generate-new-buffer ,process-buffer-name)
                  :command (list ,@backend-command buffer-temp-file)
                  :sentinel
                  (lambda (proc _event)
                    (when (memq (process-status proc) '(exit signal))
                      (unwind-protect
                          (let ((source-buffer (process-get proc 'source-buffer))
                                (report-fn (process-get proc 'report-fn))
                                (buffer-diagnostics '()))
                            (if (with-current-buffer source-buffer (eq proc ,proc-var))
                                (with-current-buffer (process-buffer proc)
                                  (goto-char (point-min))
                                  (while-let ((checker-result (,output-parse-fn)))
                                    (let* ((region (flymake-diag-region
                                                    source-buffer
                                                    (plist-get checker-result :line)
                                                    (plist-get checker-result :col)))
                                           (diag (flymake-make-diagnostic
                                                  source-buffer
                                                  (car region)
                                                  (cdr region)
                                                  ,severity
                                                  (plist-get checker-result :description))))
                                      (add-to-list 'buffer-diagnostics diag)))

                                  (kill-buffer (process-buffer proc))

                                  (ignore-errors
                                    (delete-file buffer-temp-file))

                                  (funcall report-fn buffer-diagnostics))

                              (flymake-log :warning "Canceling obsolete check %s" proc))))))))
           (process-put ,proc-var 'source-buffer (current-buffer))
           (process-put ,proc-var 'report-fn report-fn))))))

(defun flymake-python-backends/mypy (report-fn &rest _args)
  (flymake-python-backends/run-backend-for-checker
   "mypy"
   "mypy"
   ("mypy" "--show-column-numbers")
   flymake-python/mypy--proc
   (lambda ()
     (when (search-forward-regexp flymake-python/mypy--output-regex nil t)
       (let* ((line (string-to-number (match-string 2)))
              (col (string-to-number (match-string 3)))
              (type (match-string 5))
              (message (match-string 4))
              (description (format "mypy %s: %s" type message)))
         `(:line ,line :col ,col :type ,type :message ,message :description ,description))))
   :error))

(defun flymake-python-backends/flake8 (report-fn &rest _args)
  (flymake-python-backends/run-backend-for-checker
   "flake8"
   "flake8"
   ("flake8")
   flymake-python/flake8--proc
   (lambda ()
     (when (search-forward-regexp flymake-python/flake8--output-regex nil t)
       (let* ((line (string-to-number (match-string 2)))
              (col (string-to-number (match-string 3)))
              (type (match-string 4))
              (message (match-string 5))
              (description (format "flake8 %s: %s" type message)))
         `(:line ,line :col ,col :type ,type :message ,message :description ,description))))
   :error))

(defun flymake-python-backends/pylint (report-fn &rest _args)
  (flymake-python-backends/run-backend-for-checker
   "pylint"
   "pylint"
   ("pylint" "--disable=C0103")
   flymake-python/pylint--proc
   (lambda ()
     (when (search-forward-regexp flymake-python/pylint--output-regex nil t)
       (let* ((line (string-to-number (match-string 2)))
              ;; Pylint columns start at 0.
              (col (1+ (string-to-number (match-string 3))))
              (type (match-string 4))
              (message (match-string 5))
              (description (format "pylint %s: %s" type message)))
         `(:line ,line :col ,col :type ,type :message ,message :description ,description))))
   :error))

(defun ruby-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'ruby-flymake nil t))

(add-hook 'ruby-mode-hook 'ruby-setup-flymake-backend)


(provide 'flymake-python-backends)
