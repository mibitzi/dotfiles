(provide 'init-evil)

(require 'evil)
(require 'surround)

(evil-mode 1)
(global-surround-mode 1)

(add-hook 'evil-normal-state-entry-hook
          'turn-on-surround-mode)
