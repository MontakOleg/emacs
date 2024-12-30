;;; my-utils.el --- Collection of useful functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; code:

(defun command-output-to-string (command &rest args)
  "Like `shell-command-to-string' but dropping error output.
Also trims whitespace from the ends of any output."
  (string-trim
   (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process command nil '(t nil) nil args)))))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun my-join-line (&optional arg beg end)
  "Join next line into current one."
  (interactive)
  (if (use-region-p)
      (join-line 'nil (region-beginning) (region-end))
    (join-line 1)))

(defun my-kill-line-or-region ()
  "If nothing selected, kill whole line, else kill region."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(provide 'my-utils)

;;; my-utils.el ends here
