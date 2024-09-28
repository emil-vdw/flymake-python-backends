(require 'f)

(defvar-local flymake-pyton/mypy--proc nil)

(defvar-local flymake-python/mypy--temp-files '())

(defvar flymake-python/mypy-output-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\) \\(\\[.+\\]\\)$")

(defun flymake-python-backends/buffer-temp-file-name ()
  ""
  (concat
   "flymake-python-"
   (substring (secure-hash 'md5 (buffer-name))
              0 16)))

(defun flymake-python-backends/buffer-to-temp-file ()
  ""
  (let ((file-name (f-join (temporary-file-directory)
                               (flymake-python-backends/buffer-temp-file-name)))
        (write-region-inhibit-fsync t)
        (jka-compr-inhibit t))
    (make-directory (file-name-directory file-name) t)
    (write-region nil nil file-name nil 0)
    file-name))

(defun flymake-python-backends/mypy (report-fn &rest _args)
  (unless (executable-find "mypy")
    (error "Cannot find a mypy executable"))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `mypy-flymake-proc' to a different value
  ;;
  (when (process-live-p flymake-pyton/mypy--proc)
    (kill-process flymake-pyton/mypy--proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (save-restriction
    (widen)

    (let ((buffer-temp-file (flymake-python-backends/buffer-to-temp-file)))
      ;; Reset the `flymake-pyton/mypy--proc' process to a new process
      ;; calling the mypy tool.
      ;;
      (setq
       flymake-pyton/mypy--proc
       (make-process
        :name "mypy-flymake" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        ;;
        :buffer (generate-new-buffer " *mypy-flymake*")
        :command
        (list "mypy" buffer-temp-file
              "--show-column-numbers")
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `flymake-pyton/mypy--proc', which indicates that
                ;; `proc' is not an obsolete process.
                ;;

                (let ((source-buffer (process-get proc 'source-buffer))
                      (report-fn (process-get proc 'report-fn))
                      (buffer-diagnostics '()))

                  (if (with-current-buffer source-buffer (eq proc flymake-pyton/mypy--proc))
                      (with-current-buffer (process-buffer proc)
                        (goto-char (point-min))

                        (while (search-forward-regexp flymake-python/mypy-output-regex (point-max) t)
                          (let* ((line (string-to-number (match-string 2)))
                                 (col (string-to-number (match-string 3)))
                                 (type (match-string 5))
                                 (msg (match-string 4))
                                 (description (format "mypy %s: %s" type msg))
                                 (region (flymake-diag-region
                                          source-buffer
                                          line
                                          col))
                                 (diag (flymake-make-diagnostic
                                        source-buffer
                                        (car region)
                                        (cdr region)
                                        :error description)))
                            (message "%s\n%s\n%s"
                                     type msg description)
                            (add-to-list 'buffer-diagnostics diag)))

                        ;; Cleanup the temporary buffer used to hold the
                        ;; check's output.
                        ;;
                        (kill-buffer (process-buffer proc))

                        ;; Clean up the temp file just created.
                        (ignore-errors
                          (delete-file buffer-temp-file))

                        (funcall report-fn buffer-diagnostics))

                    (flymake-log :warning "Canceling obsolete check %s"
                                 proc))))))))

      (process-put flymake-pyton/mypy--proc 'source-buffer (current-buffer))
      (process-put flymake-pyton/mypy--proc 'report-fn report-fn))))

(defun ruby-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'ruby-flymake nil t))

(add-hook 'ruby-mode-hook 'ruby-setup-flymake-backend)


(provide 'flymake-python-backends)
