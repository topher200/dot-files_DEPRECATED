;; path update
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; startup with vimpulse
(require 'vimpulse)
(setq viper-mode t)

;; gui settings
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-default 'truncate-lines t) ;; stop word wrap
(setq visible-bell t) ;; stop viper-mode from making noise
(setq column-number-mode t) ;; show column number in mode bar
(blink-cursor-mode 0) ;; turn off blinking cursor

;; window keybindings
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

;; programming keybindings
(global-set-key (kbd "<f6>") 'fs-lint)
(global-set-key (kbd "<f7>") '(lambda () (interactive) (compile compile-command)))

;; movement control
(global-set-key(kbd "C-j") 'scroll-up)
(global-set-key(kbd "C-k") 'scroll-down)
(setq next-screen-context-lines 6)

;; viper settings
(setq-default viper-auto-indent t)
(setq-default viper-case-fold-search t) ;; case-insensitive search
(setq-default viper-shift-width 2)

;; misc keybindings
(global-set-key(kbd "C-s") 'save-buffer)

;; misc settings
(setq-default backup-inhibited t) ;; no backups
(global-auto-revert-mode) ;; automatically revert unmodified buffers that changed
(fset 'yes-or-no-p 'y-or-n-p) ;; make all yes/no questions y/n

;; colors
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-charcoal-black)
(set-cursor-color "red")

;; fonts
(defun my-font () (interactive)
  (set-default-font "-outline-Crisp-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1"))
;; failing my-font() silently if the font isn't installed
(condition-case nil
  (my-font)
  (error 
    (message "Topher: init.el failed to load font. It probably isn't installed")
    (set-face-attribute 'default nil :height 100)
  nil))
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

;; org-mode settings
(setq org-hide-leading-stars t)

;; opening files in an already-running instance of emacs:
(server-start)

;; start an edit server so Chrome can hook into emacs
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

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

;; farsounder specific
(require 'farsounder)
(require 'sonasoft)
(sonasoft-setup-compile)
