;;; init

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(mapc 'require '(init-bootstrap
                 init-appearance
                 init-projectile
                 init-evil
                 init-webmode
                 init-php
                 ))

;(require 'server)
;(unless (server-running-p)
;  (server-start))
