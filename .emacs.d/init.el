;;; init

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(mapc 'require '(init-bootstrap
                 init-appearance
                 init-webmode
                 init-php
                 ))

;(require 'server)
;(unless (server-running-p)
;  (server-start))
