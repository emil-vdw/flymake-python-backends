(require 'f)

(defvar-local flymake-python/mypy--proc nil)
(defvar-local flymake-python/flake8--proc nil)
(defvar-local flymake-python/pylint--proc nil)

(defvar-local flymake-python/mypy--temp-files '())

(defvar flymake-python/mypy--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.+?\\) ?\\(\\[.+\\]\\)?$"
  "Regular expression to parse 'mypy' output for file, line, column, type, and message.")

(defvar flymake-python/flake8--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\) \\(.+\\)$"
  "Regular expression to parse 'flake8' output for file, line, column, type, and message.")

(defvar flymake-python/pylint--output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.+\\)$"
  "Regular expression to parse 'pylint' output for file, line, column, type, and message.")

(defun flymake-python-backends/plist-get-required (plist prop)
  "Retrieve PROP from PLIST or signal an error if PROP is missing.

This is useful for retrieving required properties from a plist.
An error is signaled if PROP is not found in PLIST."
  (let ((value (plist-get plist prop)))
    (if value
        value
      (error "Property %s is required but missing" prop))))

(defun flymake-python-backends/buffer-temp-file-name (backend-name)
  "Generate a unique temporary file name for the current buffer using BACKEND-NAME.

This function uses the backend name and a hash of the buffer name to create the temp file."
  (concat
   "flymake-python-"
   backend-name
   (substring (secure-hash 'md5 (buffer-name))
              0 16)))

(defun flymake-python-backends/buffer-to-temp-file (backend-name)
  "Write the contents of the current buffer to a temporary file for BACKEND-NAME.

The file is stored in the system's temporary directory and is used by the backend checker.
Returns the file name of the temporary file."
  (let ((file-name (f-join (temporary-file-directory)
                               (flymake-python-backends/buffer-temp-file-name
                                backend-name)))
        (write-region-inhibit-fsync t)
        (jka-compr-inhibit t))
    (make-directory (file-name-directory file-name) t)
    (write-region nil nil file-name nil 0)
    file-name))

(defmacro flymake-python-backends/define-backend-sentinel (proc-var output-parse-fn source)
  "Define a sentinel for a process using PROC-VAR, OUTPUT-PARSE-FN, and SOURCE.

This macro sets up a sentinel that processes the output of a subprocess, parses
the diagnostics using OUTPUT-PARSE-FN, and reports them to Flymake.
SOURCE defines whether the process is checking a real file (:real-file) or a temporary one."
  `(lambda (proc _event)
     (when (memq (process-status proc) '(exit signal))
       (unwind-protect
           (let ((source-buffer (process-get proc 'source-buffer))
                 (report-fn (process-get proc 'report-fn))
                 (buffer-diagnostics '()))

             (cond
              ;; Check whether this callback is for the most recent checker
              ;; process.
              ((not (with-current-buffer source-buffer (eq proc ,proc-var)))
               (flymake-log :warning "Canceling obsolete check %s" proc))

              ;; If the file the checker is checking is the actual source file
              ;; associated with the buffer, only report diagnostics if the
              ;; buffer hasn't been modified since the check began (i.e. there
              ;; are no unsaved changes).  Otherwise the line and column numbers
              ;; will not be accurate, or we might report errors that aren't
              ;; present anymore.
              ((and (eq ,source :real-file)
                    (buffer-modified-p))
               (flymake-log :warning "Canceling report because file is not saved %s" proc))

              (t
               ;; Safe to continue parsing and reporting.
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
                                 (plist-get checker-result :severity)
                                 (plist-get checker-result :description))))

                     (add-to-list 'buffer-diagnostics diag)))

                 (kill-buffer (process-buffer proc))
                 (funcall report-fn buffer-diagnostics)))))))))

(defmacro flymake-python-backends/define-backend-for-checker (&rest properties)
  "Define a Flymake backend using the given PROPERTIES.

The properties must include at least :report-fn, :backend-name, :proc-var, and :output-parse-fn.
This macro creates a Flymake backend that runs an external checker, collects diagnostics,
and reports them to Flymake."
  (declare (indent 1))
  (let* ((report-fn (flymake-python-backends/plist-get-required properties :report-fn))
         (backend-name
          (flymake-python-backends/plist-get-required properties :backend-name))
         (executable
          (or (plist-get properties :executable)
              backend-name))
         (backend-command (or (plist-get properties :command)
                              (list executable)))
         (proc-var
          (flymake-python-backends/plist-get-required properties :proc-var))
         (output-parse-fn
          (flymake-python-backends/plist-get-required properties :output-parse-fn))
         (source (or (plist-get properties :source)
                     :temp-file))
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
         
         (let ((file-to-check
                (pcase ,source
                  (:real-file (expand-file-name buffer-file-name))
                  (:temp-file (flymake-python-backends/buffer-to-temp-file ,backend-name))
                  (_ (flymake-python-backends/buffer-to-temp-file ,backend-name)))))

           (if (or (not file-to-check)
                   (and (eq ,source :real-file)
                        (buffer-modified-p)))
               ;; When the checker process is checking the actual file
               ;; associated with the buffer, only continue if there are no
               ;; unsaved changes.
               (flymake-log :warning
                            "Canceling %s check because file is not saved"
                            ,backend-name)
             
             (setq ,proc-var
                   (make-process
                    :name ,process-name
                    :noquery t
                    :connection-type 'pipe
                    :buffer (generate-new-buffer ,process-buffer-name)
                    :command (list ,@backend-command file-to-check)
                    :sentinel
                    (flymake-python-backends/define-backend-sentinel
                        ,proc-var ,output-parse-fn ,source)))
             
             ;; Attach some important data to the process object for use by the
             ;; sentinal later.
             (process-put ,proc-var 'source-buffer (current-buffer))
             (process-put ,proc-var 'report-fn ,report-fn)))))))

(defun flymake-python-backends/mypy (report-fn &rest _args)
  "Flymake backend for 'mypy'.

REPORT-FN is the function used to report diagnostics to Flymake.
This backend checks Python files using 'mypy' and parses the output."
  (flymake-python-backends/define-backend-for-checker
      :report-fn report-fn
      :backend-name "mypy"
      :source :real-file
      :executable "mypy"
      :command ("mypy" "--show-column-numbers")
      :proc-var flymake-python/mypy--proc
      :output-parse-fn
      (lambda ()
        (when (search-forward-regexp flymake-python/mypy--output-regex nil t)
          (let* ((line (string-to-number (match-string 2)))
                 (col (string-to-number (match-string 3)))
                 (type (match-string 6))
                 (message (match-string 5))
                 (description (if type
                                  (format "mypy %s: %s" type message)
                                (format "mypy: %s" message)))
                 (severity (intern (concat ":" (match-string 4)))))
            `(:line ,line :col ,col :type ,type :message ,message :description ,description :severity ,severity))))))

(defun flymake-python-backends/flake8 (report-fn &rest _args)
  "Flymake backend for 'flake8'.

REPORT-FN is the function used to report diagnostics to Flymake.
This backend checks Python files using 'flake8' and parses the output."
  (flymake-python-backends/define-backend-for-checker
      :report-fn report-fn
      :backend-name "flake8"
      :command ("flake8")
      :proc-var flymake-python/flake8--proc
      :output-parse-fn
      (lambda ()
        (when (search-forward-regexp flymake-python/flake8--output-regex nil t)
          (let* ((line (string-to-number (match-string 2)))
                 (col (string-to-number (match-string 3)))
                 (type (match-string 4))
                 (message (match-string 5))
                 (description (format "flake8 %s: %s" type message)))
            `(:line ,line :col ,col :type ,type :message ,message :description ,description :severity :error))))))

(defun flymake-python-backends/pylint (report-fn &rest _args)
  "Flymake backend for 'pylint'.

REPORT-FN is the function used to report diagnostics to Flymake.
This backend checks Python files using 'pylint' and parses the output."
  (flymake-python-backends/define-backend-for-checker
      :report-fn report-fn
      :backend-name "pylint"
      :executable "pylint"
      :command ("pylint")
      :proc-var flymake-python/pylint--proc
      :output-parse-fn
      (lambda ()
        (when (search-forward-regexp flymake-python/pylint--output-regex nil t)
          (let* ((line (string-to-number (match-string 2)))
                 ;; Pylint columns start at 0.
                 (col (1+ (string-to-number (match-string 3))))
                 (type (match-string 4))
                 (message (match-string 5))
                 (description (format "pylint %s: %s" type message)))
            `(:line ,line :col ,col :type ,type :message ,message :description ,description :severity :error))))))

(defun flymake-python-backends/setup-mypy-flymake-backend ()
       (add-hook 'flymake-diagnostic-functions 'flymake-python-backends/mypy nil t))

(defun flymake-python-backends/setup-flake8-flymake-backend ()
       (add-hook 'flymake-diagnostic-functions 'flymake-python-backends/flake8 nil t))

(defun flymake-python-backends/setup-pylint-flymake-backend ()
       (add-hook 'flymake-diagnostic-functions 'flymake-python-backends/pylint nil t))


(provide 'flymake-python-backends)
