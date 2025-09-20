;;; dired-utils.el --- Helpful functions used in dired buffer  -*- lexical-binding: t; -*-
;;; Commentary:
;;; code:

;; https://xenodium.com/interactive-ordering-of-dired-items
;; https://github.com/xenodium/dotsies/blob/365ec799ae20ceb30658c2e4ad40da8d82bb6d87/emacs/features/fe-dired.el#L284-L339

(defun ar/dired-drag-item-up ()
    "Drag dired item down in buffer."
    (interactive)
    (unless (dired-get-filename nil t)
      (error "Not a dired draggable item"))
    (when (= (line-number-at-pos) 2)
      (error "Already at top"))
    (let* ((inhibit-read-only t)
           (col (current-column))
           (item-start (line-beginning-position))
           (item-end (1+ (line-end-position)))
           (item (buffer-substring item-start item-end)))
      (delete-region item-start item-end)
      (forward-line -1)
      (beginning-of-line)
      (insert item)
      (forward-line -1)
      (move-to-column col)))

  (defun ar/dired-drag-item-down ()
    "Drag dired item down in buffer."
    (interactive)
    (unless (dired-get-filename nil t)
      (error "Not a dired draggable item"))
    (when (save-excursion
            (forward-line 1)
            (eobp))
      (error "Already at bottom"))
    (let* ((inhibit-read-only t)
           (col (current-column))
           (item-start (line-beginning-position))
           (item-end (1+ (line-end-position)))
           (item (buffer-substring item-start item-end)))
      (delete-region item-start item-end)
      (forward-line 1)
      (beginning-of-line)
      (insert item)
      (forward-line -1)
      (move-to-column col)))

  (defun ar/dired-from-marked-items ()
    "Create a new dired buffer containing only the marked files.

Also allow dragging items up and down via M-<up> and M-x<down>."
    (interactive)
    (let ((marked-files (dired-get-marked-files))
          (buffer-name (generate-new-buffer-name
                        (format "*%s (selection)*"
                                (file-name-nondirectory
                                 (directory-file-name default-directory))))))
      (unless marked-files
        (error "No dired marked files"))
      (dired (cons buffer-name
                   (mapcar (lambda (path)
                             (file-relative-name path default-directory))
                           marked-files)))))

(provide 'dired-utils)

;;; dired-utils.el ends here
