;; -*- lexical-binding: t; -*-
(use-package multiple-cursors
  :ensure t)

(use-package boon
  :ensure t
  :demand t
  :bind
  ("C-c C-r" . madmacs-boon-reset)
  
  :config
  (boon-mode 1)
  
  (require 'boon-qwerty-hjkl)
  
  (define-key boon-goto-map ":" 'avy-goto-char)
  (define-key boon-goto-map "'" 'avy-goto-char-2)
  (define-key boon-goto-map "f" 'avy-goto-line)
  (define-key boon-goto-map "w" 'avy-goto-word-1)
  (define-key boon-goto-map "e" 'avy-goto-word-0)
  
  (define-key boon-command-map "=" 'indent-according-to-mode)
  (define-key boon-command-map "\"" boon-quote-character)
  (define-key boon-goto-map "l" 'consult-line)

  (global-set-key (kbd "C-o") 'pop-global-mark)

  (defun madmacs-boon-reset ()
    "Reset boon to default state."
    (interactive)
    (turn-off-boon-mode)
    (turn-on-boon-mode))
  
  (with-eval-after-load 'vterm
    (add-hook 'vterm-mode-hook
      (lambda ()
        (turn-off-boon-mode)))))

(provide 'madmacs-keys-boon)
