(setq viper-expert-level  '3)
(setq viper-inhibit-startup-message 't)

;; Enable vim-like window transitions
(define-key viper-vi-global-user-map "\C-wh" 'windmove-left)
(define-key viper-vi-global-user-map "\C-wj" 'windmove-down)
(define-key viper-vi-global-user-map "\C-wk" 'windmove-up)
(define-key viper-vi-global-user-map "\C-wl" 'windmove-right)

;; goto-last-change functionality
(require 'goto-last-change)
(define-key viper-vi-global-user-map "g;" 'goto-last-change)
