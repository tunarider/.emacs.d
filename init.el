(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(marginalia vertico which-key flycheck go-mode company terraform-mode dap-mode lsp-treemacs helm-lsp lsp-ui golden-ratio solaire-mode diminish vterm-toggle vterm pinentry treemacs-magit treemacs-projectile treemacs-evil treemacs ibuffer-projectile centaur-tabs highlight-indent-guides helm-projectile projectile nyan-mode doom-modeline doom-themes all-the-icons evil use-package))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "nil" :slant normal :weight normal :height 100 :width normal)))))

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)
(set-default 'truncate-lines t)

(global-set-key (kbd "M-s-h") 'windmove-left)
(global-set-key (kbd "M-s-l") 'windmove-right)
(global-set-key (kbd "M-s-k") 'windmove-up)
(global-set-key (kbd "M-s-j") 'windmove-down)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-'") 'xref-find-definitions)
(global-set-key (kbd "C-\"") 'xref-find-references)

(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
(setenv "PATH" (concat
                "/usr/local/bin:"
                (getenv "GOPATH") "/bin:"
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

(use-package all-the-icons :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config))

(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar nil)
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "+")
  (defun centaur-tabs-buffer-groups ()
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
  (centaur-tabs-mode t)
  :bind
  ("M-s-y" . centaur-tabs-backward)
  ("M-s-o" . centaur-tabs-forward)
  ("M-s-i" . centaur-tabs-backward-group)
  ("M-s-u" . centaur-tabs-forward-group))

(use-package solaire-mode
  :ensure t
  :hook (change-major-mode . turn-on-solaire-mode)
  :hook (after-revert . turn-on-solaire-mode)
  :hook (ediff-prepare-buffer . solaire-mode)
  :config
  (setq solaire-mode-auto-swap-bg nil)
  (solaire-global-mode +1))

(use-package golden-ratio :ensure t :config (golden-ratio-mode 1))

(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (setq nyan-bar-length 16)
  :config
  (progn (nyan-mode t)))

(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode)
   (terraform-mode . display-line-numbers-mode)
   (yaml-mode . display-line-numbers-mode)))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap)
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (yaml-mode . highlight-indent-guides-mode)))

(use-package projectile
  :ensure t
  :init
  (defadvice projectile-on (around exlude-tramp activate)
    (unless  (--any? (and it (file-remote-p it))
					 (list
					  (buffer-file-name)
					  list-buffers-directory
					  default-directory)) ad-do-it))
  :config
  (progn (setq projectile-enable-caching t)
	 (setq projectile-globally-ignored-buffers (quote ("*terminal*")))
	 (setq projectile-globally-ignored-directories
		'(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
	 (setq projectile-globally-ignored-modes
		'("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "term-mode" "popwin-mode"))
	 (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	 (projectile-mode t)))

(use-package helm-projectile
  :ensure t
  :after helm projectile
  :config
  (helm-projectile-on))

(use-package ibuffer :ensure t)

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

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil :after (treemacs evil) :ensure t)

(use-package treemacs-projectile :after (treemacs projectile) :ensure t)

(use-package treemacs-magit :after (treemacs magit) :ensure t)

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map ("C-c C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 100000))

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

(use-package magit :ensure t)

(use-package flycheck
  :ensure t
  :hook
  ((emacs-lisp-mode . flycheck-mode)))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.3)
  (global-company-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((terraform-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-terraform-ls-enable-show-reference t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-semantic-tokens-honor-refresh-requests t)
  (setq lsp-enable-links t)
  (setq lsp-go-gopls-server-path (concat (getenv "GOPATH") "/bin/gopls"))
  (setq lsp-disabled-clients '(tfls))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :defines
  lsp-ui-peek-always-show
  :config
  (setq lsp-ui-peek-always-show t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

(use-package dap-mode :ensure t)

(use-package go-mode :ensure t)

(use-package terraform-mode
  :ensure t
  :hook
  ((terraform-mode . terraform-format-on-save-mode))
  :config
  (setq create-lockfiles nil))

(use-package pinentry
  :ensure t)

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package marginalia :ensure t :init (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))
