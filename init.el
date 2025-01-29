;;; init.el --- My init.el file

;;; Commentary:
;;; None

;;; code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(tree-sitter treesit php-mode typescript-mode treemacs-nerd-icons treemacs-all-the-icons popwin go-mode yaml-mode all-the-icons lsp-java helm-lsp lsp-ui dap-mode yasnippet which-key pinentry company flycheck-pos-tip flycheck vterm-toggle vterm treemacs-magit treemacs-projectile treemacs-evil treemacs helm-projectile ibuffer-projectile projectile highlight-indent-guides nyan-mode solaire-mode centaur-tabs doom-modeline exec-path-from-shell))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "D2Coding" :foundry "nil" :slant normal :weight medium :height 130 :width normal)))))
 ;; '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight medium :height 130 :width normal)))))
;; |---------------|
;; | 테스트 테스트 |
;; | test          |

(unless package-archive-contents (package-refresh-contents))
;; (unless (package-installed-p 'use-package) (package-install 'use-package))

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

(setq use-package-always-ensure t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq column-number-mode t)
(setq size-indication-mode t)

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

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-fine-undo t)
  (defun my-advice-evil-delete-char (orig-fn beg end &optional type _ &rest args)
    (apply orig-fn beg end type ?_ args))
  (advice-add 'evil-delete-char :around 'my-advice-evil-delete-char)
  :config
  (evil-mode))

(use-package all-the-icons)

(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 40)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-projectile :after (treemacs projectile))

(use-package treemacs-magit :after (treemacs magit))

(use-package doom-themes
  :defines
  doom-themes-treemacs-theme
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-modeline-minor-modes t)
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  ;; https://github.com/emacs-lsp/lsp-treemacs/issues/89
  ;; (treemacs-load-all-the-icons-with-workaround-font "Hermit")
  (with-eval-after-load 'lsp-treemacs
    (treemacs-load-all-the-icons-with-workaround-font "MesloLGS NF")
    ;; (doom-themes-treemacs-config)
    ))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-nil))

(use-package centaur-tabs
  :demand
  :init
  :after projectile
  :config
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
	(list (cond ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode) "Term")
                ((derived-mode-p 'magit-mode) (concat "magit:" (magit-toplevel)))
                ((string-match-p "\*kubernetes" (buffer-name)) "Kubernetes")
				((string-match-p (rx (or
                                      "\*openfortivpn"
                                      "\*Helm"
                                      "\*helm"
                                      "\*tramp"
                                      "\*Completions\*"
                                      "\*sdcv\*"
                                      "\*Messages\*"
                                      "\*Ibuffer\*"
                                      "\*Ido Completions\*"
                                      "\*dockerfile-ls\*"
                                      "\*dockerfile-ls::stderr\*")) (buffer-name)) "Emacs")
				((projectile-project-p) (projectile-project-name))
				(t "Common"))))
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-group-buffer-groups)
  :bind
  ("M-s-y" . centaur-tabs-backward)
  ("M-s-o" . centaur-tabs-forward)
  ("M-s-i" . centaur-tabs-backward-group)
  ("M-s-u" . centaur-tabs-forward-group)
  ("M-s-g" . centaur-tabs-toggle-groups))

(use-package solaire-mode
  :defines
  solaire-mode-auto-swap-bg
  :hook ((change-major-mode . turn-on-solaire-mode)
         (after-revert . turn-on-solaire-mode)
         (ediff-prepare-buffer . solaire-mode))
  :config
  (setq solaire-mode-auto-swap-bg nil)
  (solaire-global-mode +1))

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (setq nyan-bar-length 16)
  :config
  (progn (nyan-mode t)))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (terraform-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode)))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'bitmap)
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode)))

(use-package projectile
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
  :after projectile
  :config
  (helm-projectile-on))

(use-package savehist
  :init
  (savehist-mode))

(use-package ibuffer)

(use-package ibuffer-projectile
  :after projectile ibuffer
  :functions
  ibuffer-do-sort-by-alphabetic
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package popwin
  :ensure t
  :config
  (push '(magit-status-mode :position bottom :height 50) popwin:special-display-config)
  (push '("^\\*vterm\\*.*" :regexp t :position bottom :height 50) popwin:special-display-config)
  (popwin-mode 1))

(use-package vterm
  :bind (:map vterm-mode-map ("C-c C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 100000))

(use-package vterm-toggle
  :bind
  ((:map global-map
         ("C-c v t" . vterm-toggle)
         ("C-c v e" . vterm-toggle-insert-cd)))
  :defines
  vterm-toggle--vterm-buffer-p-function
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

(use-package magit)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package company
  :diminish company-mode
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.3)
  (global-company-mode))

(use-package pinentry)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package yasnippet :config (yas-global-mode))

(use-package dap-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/Users/user/.emacs.d/extra/lombok.jar" "-Xbootclasspath/a:/Users/user/.emacs.d/extra/lombok.jar"))
  (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/openjdk-21.jdk/Contents/Home/bin/java")
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.39.0/jdt-language-server-1.39.0-202408291433.tar.gz"))

(use-package yaml-mode)

(use-package go-mode)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))


(use-package tree-sitter
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))

(load-file "~/.emacs.d/local.el")

(provide 'init)

;;; init.el ends here

