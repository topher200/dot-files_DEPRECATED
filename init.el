;; path update
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; Variables configured via 'customize' interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; startup with vimpulse
(require 'vimpulse)
(setq viper-mode t)

;; gui settings
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-default 'truncate-lines t) ;; stop word wrap
(setq visible-bell t) ;; stop viper-mode from making noise
(setq column-number-mode t) ;; show column number in mode bar
(blink-cursor-mode 0) ;; turn off blinking cursor
(fringe-mode '(0 . 0))  ; == minimal (not sure why 'minimal doesn't work)

;; window keybindings
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-s") 'split-window-horizontally)
(global-set-key (kbd "M-v") 'split-window-vertically)
(global-set-key (kbd "M-=") 'balance-windows)

;; programming keybindings
(global-set-key(kbd "C-s") 'save-buffer)
;; run the check/compile for the current language
(global-set-key(kbd "<f7>") 'check-syntax)

;; movement control
(global-set-key(kbd "C-j") 'scroll-up)
(global-set-key(kbd "C-k") 'scroll-down)

;; viper settings
(setq-default viper-auto-indent t)
(setq-default viper-case-fold-search t) ;; case-insensitive search
(setq-default viper-shift-width 2)

;; misc settings
(setq-default backup-inhibited t) ;; no backups
(setq auto-save-default nil) ;; no autosave
(global-auto-revert-mode) ;; automatically revert unmodified buffers that changed
(fset 'yes-or-no-p 'y-or-n-p) ;; make all yes/no questions y/n
(desktop-save-mode 1) ;; restore emacs state on startup

;; colors
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-greiner2)

;; fonts
(set-face-attribute 'default nil :height 100)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq case-fold-search t)
(setq current-language-environment "Latin-1")

;; color changes required for linux M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; use ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(partial-completion-mode 't)

;; use smex mode (ido for M-x)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; org-mode settings
(setq org-hide-leading-stars t)

;; start an edit server so Chrome can hook into emacs
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

;; use undo-tree instead of the standard undo
(require 'undo-tree)
(global-undo-tree-mode)
(define-key viper-vi-global-user-map "u" 'undo-tree-undo)
(define-key viper-vi-global-user-map "U" 'undo-tree-redo)

;; use goto-last-change
(require 'goto-last-change)
(define-key viper-vi-global-user-map "g;" 'goto-last-change)

;; use ack-mode
(require 'ack)

;; use w3m
(add-to-list 'load-path "~/.emacs.d/site-lisp/w3m")
(require 'w3m-ems)
(require 'w3m)

;; c++-mode
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.idl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; python-mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyw$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pxd$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx$" . python-mode))
(setq python-indent 2)

;; clojure-mode
(require 'clojure-mode)
;; slime support
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/slime"))
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))))
(require 'slime)
(slime-setup)

;; evan functions: google()
(require 'evan-functions)

;; topher-specific
(require 'topher-functions)

;; override viper-keybinding for C-t to open Chrome
(define-key viper-vi-global-user-map(kbd "C-t")
  (lambda () (interactive)(browse-url "www.google.com")(message "hi!")))
