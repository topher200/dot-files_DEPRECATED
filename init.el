;; path update
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; gui settings
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-default 'truncate-lines t) ;; stop word wrap
(setq visible-bell t) ;; stop viper-mode from making noise
(setq column-number-mode t) ;; show column number in mode bar
(blink-cursor-mode 0) ;; turn off blinking cursor

;; misc settings
(setq-default backup-inhibited t) ;; no backups

;; colors
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-charcoal-black)
(set-cursor-color "red")

;; fonts
(defun my-font () (interactive)
  (set-default-font "-outline-Crisp-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1"))
(my-font)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq case-fold-search t)
(setq current-language-environment "Latin-1")
;; (setq current-input-method "latin-1-prefix") ; failing the vimpulse load

;; startup with vimpulse
(require 'vimpulse)
(setq viper-mode t)

;; use ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(partial-completion-mode 't)

;; org-mode settings
;; startup with special indenting- don't show leading stars
(setq org-hide-leading-stars t)

;; opening files in an already-running instance of emacs:
(server-start)
