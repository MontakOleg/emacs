;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

;; (setq use-package-verbose t)

(setq native-comp-async-report-warnings-errors nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; UI

(setq save-interprogram-paste-before-kill t
      backup-by-copying t
      ring-bell-function 'ignore
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      ;; org mode
      org-support-shift-select t
      org-startup-indented t
      org-log-done 'time
      org-priority-enable-commands nil
      ;; dired
      dired-listing-switches "-lah"  ; human readable file sizes
      )

(setq-default
 indent-tabs-mode nil
 cursor-type 'bar)

;; Font
(setq-default line-spacing 0.1)
(set-face-attribute 'default nil
                    ;; :family "JetBrains Mono"
                    :family "Fira Code"
                    :height 150)

;; Maximize window
(toggle-frame-maximized)

;; Themes
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-load-theme 'modus-operandi-tinted))

;; (disable-theme 'modus-operandi-tinted)
;; (load-theme 'tango-dark)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(electric-pair-mode 1)

;; Line numbers in prog mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; Make ESC quit promts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Auto reload from disk
(global-auto-revert-mode 1)

;; Auto reload Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Save minibuffer prompts history
(setq history-length 25)
(savehist-mode 1)

;; Autosave right in files, not in #files#
(auto-save-visited-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Typing with an active selection overwrites
(delete-selection-mode t)

;; Ediff
(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

;; Store backups in user-emacs-directory/backups
(unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;; Code folding (collapse / expand)

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
(global-set-key (kbd "s-=") 'hs-show-block)
(global-set-key (kbd "s-+") 'hs-show-all)
(global-set-key (kbd "s--") 'hs-hide-block)
(global-set-key (kbd "s-_") 'hs-hide-all)

;; Minibuffer

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Quelpa

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

;; Copilot

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :hook prog-mode
  :bind (
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion))

  :config
  ;; (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
  (add-to-list 'copilot-indentation-alist '(org-mode . 2))
  ;; (add-to-list 'copilot-indentation-alist '(text-mode . 2))
  ;; (add-to-list 'copilot-indentation-alist '(closure-mode . 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2)))

;; flymake

(use-package flymake
  :hook prog-mode
  :bind (("<f2>" . flymake-goto-next-error)
         ("<S-f2>" . flymake-goto-prev-error)))

;; vertico

(use-package vertico
  :config
  (setq vertico-resize nil)
  (vertico-mode 1))

(use-package corfu
  ;; :custom (corfu-auto nil)
  ;; :bind
  ;; (:map corfu-map ("RET" . nil)) ; don't use RET for completion
  :init
  (global-corfu-mode))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-x b" . consult-buffer)))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult)

(recentf-mode 1)

;; languagetool

(use-package languagetool
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "/opt/homebrew/Cellar/languagetool/6.4/libexec/languagetool-commandline.jar"
        languagetool-server-command "/opt/homebrew/Cellar/languagetool/6.4/libexec/languagetool-server.jar"))

;; ruby

(use-package ruby-mode
  :mode ("Fastfile" "Appfile" "Scanfile" "Matchfile" "Gemfile"))

;; swift

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :interpreter "swift")

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . my-swift-mode:eglot-server-contact)))

(use-package ob-swift
  :after org)

;; http requests

(use-package ob-http
  :after org)

;; org

(use-package org
  :ensure nil
  :custom (org-confirm-babel-evaluate nil))

(use-package ob
  :ensure nil
  :config
  (add-to-list 'org-src-lang-modes '("swift" . swift))
  (org-babel-do-load-languages 'org-babel-load-languages '((swift . t)
                                                           (http . t)
                                                           (shell . t))))

;; yaml

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

;; gptel

(use-package gptel
  :bind
  ("C-c RET" . gptel-send))

;; helpful

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; magit

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk 'all))

;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

(use-package expand-region
  :bind (("M-<up>" . 'er/expand-region)
	 ("M-<down>" . 'er/contract-region))
  :defer t)

;; multiple-cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this-dwim)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
         ("C-M->" . 'mc/unmark-next-like-this)
         :map mc/keymap
         ("<return>" . nil)
         ("<escape>" . mc/keyboard-quit)))

;; dumb-jump

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'ag))

;; wgrep

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit))
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package markdown-mode)

;; vterm
(use-package vterm)

;; exec-path-from-shell

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Utils

(load (expand-file-name "startup/utils.el" user-emacs-directory))

(add-to-list 'load-path (concat user-emacs-directory "localpackages"))

;; kotlin-ts-mode

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

;; xcode-build

(use-package xcode-build
  :ensure nil
  :after swift-mode)

;;; Xcode + Eglot

(defun my-swift-mode:xcrun (&rest args)
  "Invoke xcrun with the given ARGS.

The result is returned as a string."
  (apply #'command-output-to-string "xcrun" args))

;;;###autoload
(defun my-swift-mode:eglot-server-contact (_ignored)
  "Locate the sourcekit-lsp executable in the active Xcode installation and return its path."
  (list (my-swift-mode:xcrun "--find" "sourcekit-lsp")))

;;; End of Xcode + Eglot

;; dired

(defun dired-mode-setup ()
  "Setup for dired mode."
  (dired-hide-details-mode t))

(add-hook 'dired-mode-hook 'dired-mode-setup)

;; Keybindings

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(bind-keys
 ("s-Z" . undo-redo)
 ("C-J" . my-join-line)
 ("s-x" . my-kill-line-or-region)
 ("s-O" . project-find-file)
 ("M-s-l" . indent-region)
 ("s-<f12>" . consult-imenu)
 ("s-d" . duplicate-line-or-region)
 ("s-f" . consult-line)
 ("s-F" . consult-ripgrep)
 ("M-z" . zap-up-to-char))

;;;

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 4 1024 1024))

;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" default))
 '(package-selected-packages
   '(corfu gptel ob-http ob-swift kotlin-ts-mode editorconfig copilot quelpa-use-package quelpa exec-path-from-shell languagetool markdown-mode vterm embark-consult embark orderless consult marginalia vertico modus-themes wgrep dumb-jump multiple-cursors expand-region evil-nerd-commenter magit helpful yaml-mode swift-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
