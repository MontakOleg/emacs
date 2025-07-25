;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (setq use-package-verbose t)

(setq native-comp-async-report-warnings-errors nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; Sane defaults

(setq save-interprogram-paste-before-kill t
      backup-by-copying t
      ring-bell-function 'ignore
      inhibit-startup-screen t
      kill-do-not-save-duplicates t
      scroll-conservatively 101             ; Scroll by one line
      help-window-select t                  ; Switching the focus to the help window when it's opened.
      global-auto-revert-non-file-buffers t ; Auto reload Dired and other buffers
      use-dialog-box nil                    ; Don't pop up UI dialog when prompting
      enable-recursive-minibuffers t
      tab-always-indent 'complete
      )

(setopt use-short-answers t
        isearch-lazy-count t)

;; Some variables are buffer-local, so changing them using setq will
;; only change them in a single buffer. Using setq-default we change
;; the buffer-local variable’s default value.

(setq-default indent-tabs-mode nil
              cursor-type 'bar
              initial-scratch-message ""
              tab-width 4
              display-line-numbers-width 3
              )

;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150)

;; Themes
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-load-theme 'modus-operandi-tinted))

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode)
  (set-face-background hl-line-face "gray91"))

;; Disable mouse-wheel-text-scale on Control+wheel
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line numbers in prog mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; Default modes

(savehist-mode 1)           ; Save minibuffer prompts history
(global-auto-revert-mode 1) ; Auto reload from disk
(auto-save-visited-mode 1)  ; Autosave right in files, not in #files#
(delete-selection-mode t)   ; Typing with an active selection overwrites
(column-number-mode 1)      ; Display columns in mode line
(pixel-scroll-precision-mode 1) ; Smooth scrolling
(minibuffer-depth-indicate-mode 1)
(recentf-mode 1)
(context-menu-mode 1)

;; EditorConfig

(use-package diminish)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

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

;;; Packages

(require 'package)
(setopt package-install-upgrade-built-in t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Local packages bootstrap

(add-to-list 'load-path (concat user-emacs-directory "localpackages"))

;;; Copilot

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :branch "main")
  ;; :hook prog-mode
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(sql-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(text-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

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

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package marginalia
  :after vertico
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

;; restclient
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

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

;; jinx

(use-package jinx
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct))
  :custom
  (jinx-languages "en_US ru"))

;; rainbow-mode

(defun add-hexdecimal-color-hook()
  (if rainbow-mode
      (font-lock-add-keywords nil hexdecimal-rainbow-font-lock-keywords 'end)
    (font-lock-remove-keywords nil hexdecimal-rainbow-font-lock-keywords)
    )
  )

(defvar hexdecimal-rainbow-font-lock-keywords
       '(
         ("0x\\([0-9a-fA-F]\\{6\\}\\)\\b" 1 (rainbow-colorize-hexadecimal-without-sharp))
         )
       "Font-lock keywords to add for 0xRRGGBB colors.")

(defun enable-rainbow-mode-for-palette-files ()
  "Enable 'rainbow-mode' for specific Swift files."
  (let ((filename (buffer-file-name)))
    (when (and filename
               (string-match-p "\\(Palette.swift\\|DSPalette.swift\\)$" filename))
      (rainbow-mode 1))))


(use-package rainbow-mode
  :hook (swift-mode . enable-rainbow-mode-for-palette-files)
  :config
  (add-hook 'rainbow-keywords-hook #'add-hexdecimal-color-hook))

;; ruby

(use-package ruby-mode
  :mode ("Fastfile" "Appfile" "Scanfile" "Matchfile" "Gemfile"))

;; go

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

;; rust

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode))

;; formatter

(defun swiftformat-buffer ()
  "Format the current buffer using swiftformat."
  (interactive)
  (when (eq major-mode 'swift-mode)
    (let ((current-file (buffer-file-name)))
      (when current-file
        (save-buffer)
        (shell-command (format "swiftformat --quiet %s" (shell-quote-argument current-file)))
        (revert-buffer t t t)))))

;; swift

(use-package flymake-swiftlint
  :ensure nil)

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :hook ((swift-mode . flymake-swiftlint-setup))
  :custom (swift-mode:parenthesized-expression-offset 4)
  :bind ( :map swift-mode-map
          ("M-s-l" . swiftformat-buffer))
  :interpreter "swift")

(defun my/setup-eglot-flymake-backend ()
    "Enable eglot's flymake backend manually."
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

(use-package eglot
  :hook ((swift-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (kotlin-ts-mode . (lambda ()
                             ;; kotlin-language-server is slow, give it 10 minutes to start
                             ;; https://github.com/fwcd/kotlin-language-server/issues/510
                             (setq-local eglot-connect-timeout 600)
                             (eglot-ensure))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (add-hook 'eglot-managed-mode-hook #'my/setup-eglot-flymake-backend)
  ;; manually manage flymake, otherwise eglot will disable all other backends
  ;; https://github.com/joaotavora/eglot/issues/268
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs
               '(swift-mode . my-swift-mode:eglot-server-contact)))

(use-package eglot-booster
    :vc (:url "https://github.com/jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(use-package ob-swift
  :after org)

;; http requests

(use-package ob-http
  :after org)

;; org

(use-package org
  :ensure nil
  :defer t
  :custom (org-confirm-babel-evaluate nil)
  :config
  (setq
      org-support-shift-select t
      org-startup-indented t
      org-priority-enable-commands nil))

(use-package ob
  :ensure nil
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("swift" . swift))
  (org-babel-do-load-languages 'org-babel-load-languages '((swift . t)
                                                           (http . t)
                                                           (sql . t)
                                                           (python . t)
                                                           (sqlite . t)
                                                           (emacs-lisp . t)
                                                           (shell . t))))

;; sql

(use-package ejc-sql
  :custom
  (ejc-jdbc-drivers
   '("sqlite"     [org.xerial/sqlite-jdbc "3.46.0.1"]
     "postgresql" [postgresql/postgresql "9.3-1102.jdbc41"]))
  (clomacs-httpd-default-port 8099)
  :after (sql-mode org-mode))

;; yaml

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

;;; gptel

(setq auth-sources '("~/.authinfo"))

(use-package gptel
  :bind
  ("C-c RET" . gptel-send)
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key-from-auth-source))
  (setq gptel-model 'claude-sonnet-4-20250514)

  ;; https://github.com/karthink/gptel/discussions/663
  (gptel-make-anthropic "Claude-thinking"
    :key #'gptel-api-key-from-auth-source
    :stream t
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "pdfs-2024-09-25")
                           ("anthropic-beta" . "output-128k-2025-02-19")
                           ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 8192) :max_tokens 32000)))

;; helpful

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; smerge
(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

;; magit

(use-package magit
  :commands (magit-status)
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk 'all))

(use-package git-pile
  :ensure nil
  :after magit
  :bind (:map magit-mode-map
              ("C-c p" . git-pile-prefix)))

;; git-link

(use-package git-link)

;; project

(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

;; expand-region

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
  (setq dumb-jump-force-searcher 'rg))

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

(use-package git-modes)

;; exec-path-from-shell

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Utils

(use-package my-utils
  :ensure nil)

;; kotlin-ts-mode

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

;; xcode-build

(use-package xcode-build
  :ensure nil
  :after swift-mode)

;;; Xcode + Eglot

(defun my-swift-mode:eglot-server-contact (_ignored)
  "Locate the sourcekit-lsp executable in the active Xcode installation and return its path."
  (if (string-equal (buffer-name) "Package.swift")
    (list (my/command-output-to-string "mise" "which" "package-swift-lsp"))
    (list (my/command-output-to-string "xcrun" "--find" "sourcekit-lsp"))))

;;; xref-eglot+dumb-jump

(defun xref-eglot+dumb-backend ()
  "Return the xref backend for eglot+dumb."
  'eglot+dumb)

(advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  "Return the identifier at point for eglot+dumb."
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  (xref-backend-apropos 'eglot pattern))

;;; End of Xcode + Eglot

;; breadcrumb

(use-package breadcrumb
  :custom
  (breadcrumb-project-max-length 0.5)
  :hook (git-commit-mode . (lambda () (breadcrumb-local-mode -1)))
  :config
  (breadcrumb-mode))

;; dired

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-use-ls-dired nil)
  (dired-listing-switches "-lah")  ; human readable file sizes
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always))

;; move-text

(use-package move-text
  :bind (("S-M-<down>" . move-text-down)
         ("S-M-<up>" . move-text-up)))

;;; mise

(use-package mise
  :diminish mise-mode
  :hook (after-init . global-mise-mode))

;; diff-hl

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

;;; yasnippet

(use-package yasnippet
  :bind (:map yas-keymap
              ("RET" . yas-next-field-or-maybe-expand))
  :config
  (yas-global-mode 1))

;;; nerd-icons

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;;; tramp

(use-package tramp
  :ensure nil
  :config

  ;; Disable version control to avoid delays
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

  (setq remote-file-name-inhibit-locks t))

;;; which-key

(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :defer 3
  :config
  (which-key-mode))

;; Keybindings

(bind-keys
 ("s-Z" . undo-redo)
 ("C-J" . my/join-line)
 ("s-x" . my/kill-line-or-region)
 ("s-O" . project-find-file)
 ("M-s-l" . indent-region)
 ("s-<f12>" . consult-imenu)
 ("s-d" . my/duplicate-line-or-region)
 ("s-f" . consult-line)
 ("s-F" . consult-ripgrep)
 ("<M-backspace>" . my/backward-delete-word)
 ("s-/" . my/comment-or-uncomment))

;; start server

(require 'server)

(unless (server-running-p)
  (server-start))

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
   '("c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
     "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2"
     default))
 '(package-selected-packages
   '(all-the-icons-dired-mode breadcrumb cape copilot corfu diff-hl
                              diminish dumb-jump eglot eglot-booster
                              ejc-sql embark-consult
                              exec-path-from-shell expand-region
                              faceup git-link git-modes go-mode gptel
                              helpful idlwave jinx kotlin-ts-mode
                              languagetool marginalia mise
                              modus-themes move-text multiple-cursors
                              nerd-icons nerd-icons-completion
                              nerd-icons-dired ob-http ob-swift
                              orderless rainbow-mode restclient
                              swift-mode tramp verilog-mode vertico
                              vterm wgrep which-key yaml-mode
                              yasnippet))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
