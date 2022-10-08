;;; === for use on mac ================
(add-to-list 'exec-path "~/homebrew/bin")
;;; === basic settings ================
(setq delete-old-versions -1
      version-control t
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.emacs_backups"))
      vc-follow-symlinks t
      auto-save-file-name-transforms '((".*" "~/.emacs.default/auto-save-list/" t))
      ring-bell-function 'ignore
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      default-fill-column 80)
(setq-default
 indent-tabs-mode nil
 c-basic-indent 2
 c-basic-offset 2
 tab-width 2)
(tool-bar-mode -1)
(global-auto-revert-mode t)
(show-paren-mode)
;; trying to set better tab stops
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))
(setq tab-stop-list (my-generate-tab-stops))

;;; === straight for packages ================
(setq straight-use-package-by-default t)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

;;; === always evil ================
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :custom (evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init)
  (evil-collection-buff-menu-setup))
(use-package evil-org
  :commands evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))

;;; === all the org ===============
(use-package org
  :straight (:type built-in)
  :config
  (setq
   org-hide-leading-stars t
   org-src-fontify-natively t
   org-export-allow-bind-keywords t
   org-directory "~/org"
        org-ellipsis "⤵"))
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))
(setq org-attach-store-link-p 'attached)
;;; === keybinding things ================
(use-package which-key
  :config
  (which-key-mode)
  (setq awhich-key-idle-delay .5))
;; custom code for faster reload
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.default/init.el"))
;;; general 
(use-package general
  :demand t
  :config
    (general-evil-setup t))
(general-nmap "," (general-simulate-key "C-c"))
;;; === C-c binding =====
(general-define-key
 :prefix "C-c"
 ;; bind to simple key press
  "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
  "/"   'counsel-git-grep   ; find string in git project
  ;; bind to double key press
  "ff"  'counsel-find-file  ; find file using ivy
  "fr"	'counsel-recentf    ; find recently edited files
  "pf"  'counsel-git        ; find file in git project
  "v"   'ivy-push-view      
  "V"   'ivy-pop-view      
  )
;;; === SPC binding =====
(general-define-key
  :keymaps '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "/" 'swiper
  "e" '(:ignore t :which-key "edit")
  "eR" 'reload-init
  )

(general-create-definer my-leader-def
  :prefix "SPC m")
(my-leader-def
  :keymaps 'normal
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture)
(general-setq evil-search-module 'evil-search)
(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  :config
  (setq key-chord-two-keys-delay 0.5))
(general-define-key :keymaps 'evil-insert-state-map
                    (general-chord "jk") 'evil-normal-state)
;;; === theme time ================
(use-package material-theme)
(use-package zenburn-theme)
(use-package solarized-theme)
;; (load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'material-light t)
(load-theme 'material t)
(use-package all-the-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;;; === assorted other packages ================
(use-package ivy
  :general
  ("C-s" 'swiper-isearch))
(use-package counsel
  :general
  ("M-x" 'counsel-M-x))
(use-package counsel-projectile)
(use-package swiper)
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;;
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-idle-delay 0.5))
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
(use-package flycheck)

;; magit
(use-package magit
  :general
  ("C-c g" 'magit-status)
  ("C-c G" 'magit-file-dispatch))
;; projects
(use-package projectile
  :init
  (projectile-mode +1)
  :general
  ("C-c p" '(:keymap projectile-command-map :package projectile))
  :bind (:map projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))
(use-package ace-window
  :init
  (setq aw-dispatch-always t)
  :general
  ("C-c w" 'ace-window))
(use-package vterm)
;;; === programming ================
(use-package lsp-mode
    :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
;;; === python ================
;; python
(use-package elpy
  :init
  (elpy-enable))
(use-package company-jedi
  :config
  (add-to-list 'company-backend 'company-jedi))
(use-package pyvenv
  :hook ((python-mode . pyvenv-mode)))
;;; === rust ================
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
;;; init.el ends here
