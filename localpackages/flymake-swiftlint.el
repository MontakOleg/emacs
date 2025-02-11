;;; flymake-swiftlint.el --- SwiftLint backend for Flymake -*- lexical-binding: t -*-

;;; Commentary:

;; Reports SwiftLint warnings via Flymake.
;; https://github.com/realm/SwiftLint

;;; Code:

(defun swiftlint-build-command (file)
  "Build the SwiftLint command list for linting FILE.
Checks for .swiftlint.yml in either project root (via project.el) or file's directory."
  (let* ((file-dir (file-name-directory file))
         (project-root (when (fboundp 'project-root)
                         (when-let ((proj (project-current)))
                           (expand-file-name (project-root proj)))))
         (config-file (or
                       (when (and project-root
                                  (file-exists-p (expand-file-name ".swiftlint.yml" project-root)))
                         (expand-file-name ".swiftlint.yml" project-root))
                       (when (file-exists-p (expand-file-name ".swiftlint.yml" file-dir))
                         (expand-file-name ".swiftlint.yml" file-dir)))))
    (append '("swiftlint" "lint" "--quiet" "--reporter" "json")
            (when config-file
              (list "--config" config-file))
            (list file))))

(defun swiftlint-parse-output (output buffer report-fn)
  "Parse the JSON OUTPUT from SwiftLint, generate diagnostics for BUFFER.
Calls REPORT-FN with the resulting diagnostics.
Any errors during parsing are silently ignored."
  (let (diags)
    (ignore-errors
      (let ((violations (json-parse-string output
                                           :object-type 'alist
                                           :array-type 'list)))
        (when violations
          (dolist (violation violations)
            (let* ((line (alist-get 'line violation))
                   (char (let ((c (alist-get 'character violation)))
                           (if (or (null c) (eq c :null))
                               nil
                             c)))
                   (severity (alist-get 'severity violation))
                   (region (flymake-diag-region buffer (1- line) char)))
              (push (flymake-make-diagnostic
                     buffer
                     (car region)
                     (cdr region)
                     (if (string= (downcase severity) "error") :error :warning)
                     (alist-get 'reason violation))
                    diags))))))

    (funcall report-fn (nreverse diags))))

(defun swiftlint-sentinel (buffer report-fn proc)
  "Process the completion of PROC for SwiftLint linting in BUFFER.
After collecting and parsing the output, REPORT-FN is invoked with
the diagnostics."
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (let ((output (buffer-string)))
        (kill-buffer (current-buffer))
        (swiftlint-parse-output output buffer report-fn)))))

(defun flymake-swiftlint (report-fn &rest _args)
  "Asynchronous SwiftLint Flymake backend using the current file.
Only runs on saved files. REPORT-FN is the callback used by Flymake.
If a .swiftlint.yml file is found in a parent directory, that configuration is used."
  (let ((executable (executable-find "swiftlint"))
        (file (buffer-file-name))
        (buffer (current-buffer)))
    (if (or (not executable)
            (not file)
            (buffer-modified-p))
        (funcall report-fn nil)
      (let ((cmdlist (swiftlint-build-command file)))
        (make-process
         :name "flymake-swiftlint"
         :noquery t
         :buffer (generate-new-buffer "*swiftlint*")
         :command cmdlist
         :sentinel (lambda (proc _event)
                     (swiftlint-sentinel buffer report-fn proc)))))))

(defun flymake-swiftlint-setup ()
    "Add SwiftLint Flymake backend."
    (when (executable-find "swiftlint")
      (add-hook 'flymake-diagnostic-functions #'flymake-swiftlint nil t)))

(provide 'flymake-swiftlint)

;;; flymake-swiftlint.el ends here
