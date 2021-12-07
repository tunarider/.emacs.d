(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" default))
 '(indent-tabs-mode nil)
 '(lsp-go-env
   #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ()))
 '(mac-command-modifier 'super)
 '(mac-option-modifier 'meta)
 '(package-selected-packages
   '(solaire-mode lsp-mode vterm-toggle vterm pinentry golden-ratio virtualenvwrapper flycheck-popup-tip groovy-mode dockerfile-mode terraform-mode diminish jinja2-mode lsp-treemacs helm-lsp lsp-ui lsp-python-ms typescript-mode vue-mode rust-mode go-mode php-mode python-mode yaml-mode mmm-mode treemacs-magit treemacs-projectile treemacs-evil treemacs flymake-diagnostic-at-point company ibuffer-projectile centaur-tabs flycheck rainbow-delimiters highlight-indent-guides helm-projectile projectile popwin multi-term yasnippet smartparens highlight-parentheses nyan-mode doom-modeline doom-themes all-the-icons evil use-package))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "nil" :slant normal :weight normal :height 100 :width normal))))
 '(flycheck-error ((t (:background "dark red" :underline t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red4" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DarkOrange4" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "gold3" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "chartreuse4" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan4" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue4" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta4" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "PaleVioletRed4" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "LightPink4" :weight bold)))))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(set-default 'tab-width 4)
(set-default 'truncate-lines t)

(global-set-key (kbd "M-s-h") 'windmove-left)
(global-set-key (kbd "M-s-l") 'windmove-right)
(global-set-key (kbd "M-s-k") 'windmove-up)
(global-set-key (kbd "M-s-j") 'windmove-down)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-'") 'xref-find-definitions)
(global-set-key (kbd "C-\"") 'xref-find-references)

(setenv "WORKON_HOME" (concat (getenv "HOME") "/.venv"))
(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
(setenv "DYLD_LIBRARY_PATH" (concat (getenv "DYLD_LIBRARY_PATH")))
(setenv "PATH" (concat
                (getenv "HOME") "/.nvm/versions/node/v12.20.0/bin:"
                (getenv "GOPATH") "/bin:"
		        (getenv "HOME") "/.cargo/bin:"
		        "/usr/local/bin:"
                "/opt/homebrew/bin/:"
		        (getenv "PATH")))

(use-package evil
  :ensure t
  :init
  (setq evil-want-fine-undo t)
  (defun my-advice-evil-delete-char (orig-fn beg end &optional type _ &rest args)
    (apply orig-fn beg end type ?_ args))
  (advice-add 'evil-delete-char :around 'my-advice-evil-delete-char)
  :config
  (evil-mode))

(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))
(advice-add 'kill-buffer :around #'yes-or-no-p->-y-or-n-p)

(use-package frame
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode))

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package tramp
  :config
  (progn (setq remote-file-name-inhibit-cache nil)
	 (setq vc-ignore-dir-regexp
	       (format "%s\\|%s"
		       vc-ignore-dir-regexp
		       tramp-file-name-regexp))
	 (setq tramp-verbose 1)))

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :defines
  doom-themes-enable-bold
  doom-themes-enable-italic
  doom-themes-treemacs-theme
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-one)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-minor-modes t))

(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (setq nyan-bar-length 16)
  :config
  (progn (nyan-mode t)))

(use-package paren
  :config
  (show-paren-mode t))

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode t))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package multi-term
  :ensure t
  :config
  (progn (setq multi-term-scroll-show-maximum-output t)
	 (setq multi-term-scroll-to-bottom-on-output t)))

(use-package popwin
  :ensure t
  :after multi-term
  :config
  (progn (push "*Shell Command Output*" popwin:special-display-config)
	 (push '(dired-mode :position top) popwin:special-display-config)
	 (popwin-mode t)))

(use-package projectile
  :ensure t
  :init
  (defadvice projectile-on (around exlude-tramp activate)
    (unless  (--any? (and it (file-remote-p it))
		     (list
		      (buffer-file-name)
		      list-buffers-directory
		      default-directory))
      ad-do-it))
  :config
  (progn (setq projectile-enable-caching t)
	 (setq projectile-globally-ignored-buffers (quote ("*terminal*")))
	 (setq projectile-globally-ignored-directories
	       (quote
		(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work")))
	 (setq projectile-globally-ignored-modes
	       (quote
		("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "term-mode" "popwin-mode")))
	 (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	 (projectile-mode t)))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config
  (helm-projectile-on))

(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode)
   (terraform-mode . display-line-numbers-mode)
   (yaml-mode . display-line-numbers-mode)))

(use-package highlight-indent-guides
  :ensure t
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (yaml-mode . highlight-indent-guides-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package flycheck
  :ensure t
  :hook
  ((emacs-lisp-mode . flycheck-mode))
  :config
  (progn  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
	  (setq flycheck-emacs-lisp-load-path 'inherit)))

;; (use-package flycheck-popup-tip
;;   :ensure t
;;   :after flycheck
;;   :hook
;;   ((flycheck-mode . flycheck-popup-tip-mode)))

(defun my-centaur-tab-buffer-groups ()
  (list (cond ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
               "Term")
              ((string-match-p (rx (or
                                    "\*Helm"
                                    "\*helm"
                                    "\*tramp"
                                    "\*Completions\*"
                                    "\*sdcv\*"
                                    "\*Messages\*"
                                    "\*Ido Completions\*")) (buffer-name))
               "Emacs")
              ((projectile-project-p)
               (projectile-project-name))
              (t "Common"))))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar nil)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "+")
  (setq centaur-tabs-buffer-groups-function 'my-centaur-tab-buffer-groups)
  (centaur-tabs-mode t)
  :bind
  ("M-s-y" . centaur-tabs-backward)
  ("M-s-o" . centaur-tabs-forward)
  ("M-s-i" . centaur-tabs-backward-group)
  ("M-s-u" . centaur-tabs-forward-group))

(use-package ibuffer
  :ensure t)

(use-package ibuffer-projectile
  :ensure t
  :after projectile ibuffer
  :functions
  ibuffer-do-sort-by-alphabetic
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.3)
  (global-company-mode))

(use-package flymake
  :ensure t)

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package treemacs
  :ensure t
  :defer t
  :defines
  winum-keymap
  :functions
  treemacs-follow-mode
  treemacs-filewatch-mode
  treemacs-fringe-indicator-mode
  treemacs-git-mode
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
               treemacs-deferred-git-apply-delay      0.5
               treemacs-display-in-side-window        t
               treemacs-eldoc-display                 t
               treemacs-file-event-delay              5000
               treemacs-file-follow-delay             0.2
               treemacs-follow-after-init             t
               treemacs-git-command-pipe              ""
               treemacs-goto-tag-strategy             'refetch-index
               treemacs-indentation                   2
               treemacs-indentation-string            " "
               treemacs-is-never-other-window         nil
               treemacs-max-git-entries               5000
               treemacs-missing-project-action        'ask
               treemacs-no-png-images                 nil
               treemacs-no-delete-other-windows       t
               treemacs-project-follow-cleanup        nil
               treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
               treemacs-position                      'left
               treemacs-recenter-distance             0.1
               treemacs-recenter-after-file-follow    nil
               treemacs-recenter-after-tag-follow     nil
               treemacs-recenter-after-project-jump   'always
               treemacs-recenter-after-project-expand 'on-distance
               treemacs-show-cursor                   nil
               treemacs-show-hidden-files             t
               treemacs-silent-filewatch              nil
               treemacs-silent-refresh                nil
               treemacs-sorting                       'alphabetic-asc
               treemacs-space-between-root-nodes      t
               treemacs-tag-follow-cleanup            t
               treemacs-tag-follow-delay              1.5
               treemacs-width                         35)
         (treemacs-resize-icons 16)
         (treemacs-follow-mode t)
         (treemacs-filewatch-mode t)
         (treemacs-fringe-indicator-mode t)
         (pcase (cons (not (null (executable-find "git")))
                      (not (null treemacs-python-executable)))
           (`(t . t)
            (treemacs-git-mode 'deferred))
           (`(t . _)
            (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package mmm-mode
  :ensure t
  :defines
  mmm-js-mode-enter-hook
  mmm-typescript-mode-enter-hook
  :config
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package yaml-mode
  :ensure t)

(use-package python-mode
  :ensure t)

(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-mode)
	 ("\\.lib\\'" . php-mode)
	 ("\\.act\\'" . php-mode)))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package vue-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
  :ensure t
  :hook
  ((php-mode . lsp)
   (rust-mode . lsp)
   (vue-mode . lsp)
   (typescript-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp))
  :defines
  lsp-go-gopls-server-path
  lsp-ui-peek-always-show
  :config
  (setq lsp-ui-peek-always-show t)
  (setq lsp-python-ms-python-executable-cmd "python3")
  (setq lsp-go-gopls-server-path (concat (getenv "GOPATH") "/bin/gopls"))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (bind-key "C-'" 'lsp-ui-peek-find-definitions)
  (bind-key "C-\"" 'lsp-ui-peek-find-references))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package jinja2-mode
  :ensure t
  :config
  (add-hook 'jinja2-mode-hook
            (lambda ()
              (remove-hook 'after-save-hook 'jinja2-indent-buffer t))))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'yas-minor-mode)
  (diminish 'highlight-indent-guides-mode)
  (diminish 'smartparens-mode)
  (diminish 'highlight-parentheses-mode)
  (diminish 'undo-tree-mode)
  (diminish 'eldoc-mode))

(use-package terraform-mode
  :ensure t
  :hook
  ((terraform-mode . terraform-format-on-save-mode)))

(use-package dockerfile-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package solaire-mode
  :ensure t
  :hook (change-major-mode . turn-on-solaire-mode)
  :hook (after-revert . turn-on-solaire-mode)
  :hook (ediff-prepare-buffer . solaire-mode)
  :config
  (setq solaire-mode-auto-swap-bg nil)
  (solaire-global-mode +1))

(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location (getenv "WORKON_HOME")))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 0))

(use-package pinentry
  :ensure t)

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map ("C-c C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 100000))

(defun my-term-mode-p(&optional args)
  (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))

(use-package vterm-toggle
  :ensure t
  :bind
  ((:map global-map
         ("C-c v t" . vterm-toggle)
         ("C-c v e" . vterm-toggle-cd)))
  :config
  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.4))))

(use-package magit
  :ensure t)
