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
;; failing my-font() silently if the font isn't installed
(condition-case nil
  (my-font)
  (error 
    (message "Topher: init.el failed to load font. It probably isn't installed")
  nil))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq case-fold-search t)
(setq current-language-environment "Latin-1")
(set-face-attribute 'default nil :height 100)

;; color changes required for linux M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; startup with vimpulse
(require 'vimpulse)
(setq viper-mode t)

;; use ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(partial-completion-mode 't)

;; org-mode settings
(setq org-hide-leading-stars t)

;; opening files in an already-running instance of emacs:
(server-start)
