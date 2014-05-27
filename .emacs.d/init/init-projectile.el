(provide 'init-projectile)

(projectile-global-mode)

(require 'ido-vertical-mode)
(require 'flx-ido)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(setq ido-enable-flex-matching t)
