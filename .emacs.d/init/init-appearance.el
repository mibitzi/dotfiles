(provide 'init-appearance)

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(global-linum-mode 1)
(menu-bar-mode -1)

(require 'color-theme)
(color-theme-initialize)
;(color-theme-calm-forest)

(load-theme 'zenburn t)
