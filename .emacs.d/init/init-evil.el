(provide 'init-evil)

(require 'evil)
(require 'surround)
(require 'evil-leader)

(evil-mode 1)
(global-surround-mode 1)
(global-evil-leader-mode)

(add-hook 'evil-normal-state-entry-hook
          'turn-on-surround-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'projectile-find-file
  "e" 'dired-jump
  "w" 'save-buffer
  "b" 'buffer-menu
  "x" 'execute-extended-command)


;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
