;;; git-pile.el --- Git-pile in magit -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs wrapper for git-pile - stacked diff support for GitHub workflows.
;; See https://github.com/keith/git-pile

;;; Code:

(require 'magit)

(transient-define-prefix git-pile-prefix ()
  "Git-pile for submit pull requests.
See https://github.com/keith/git-pile"
  ["Actions"
   ("p" "Submit PR" git-pile-submitpr)
   ("u" "Update PR" git-pile-updatepr)
   ("U" "Update PR by squash" git-pile-updatepr-squash)
   ("r" "Rebase PR" git-pile-rebasepr)
   ("R" "Replace PR" git-pile-replacepr)
   ("k" "Delete branch" git-pile-cleanupbranch)])

(defun git-pile-submitpr (commit)
  "Submit new pull request from selected COMMIT."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "submitpr" commit))

(defun git-pile-updatepr (commit)
  "Update existing pull request with selected COMMIT."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "updatepr" commit))

(defun git-pile-updatepr-squash (commit)
  "Update existing pull request with selected COMMIT by squash."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "updatepr" commit "--squash"))

(defun git-pile-rebasepr (commit)
  "Rebase existing pull request created from COMMIT to origin."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "rebasepr" commit))

(defun git-pile-replacepr (commit)
  "Entirely replace the contents of the underlying branch with a new COMMIT."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "replacepr" commit))

(defun git-pile-cleanupbranch (commit)
  "Remove local and remote branch created from COMMIT."
  (interactive (list (magit-commit-at-point)))
  (when (not commit) (user-error "No commit selected"))
  (magit-run-git-async "pilecleanupremotebranch" commit))

(provide 'git-pile)
;;; git-pile.el ends here
