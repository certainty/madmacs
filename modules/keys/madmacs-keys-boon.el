;; -*- lexical-binding: t; -*-
(use-package multiple-cursors
  :ensure t)

(use-package swiper
  :ensure t)

(use-package better-jumper
  :ensure t
  :commands (better-jumper-jump-backward better-jumper-jump-forward)
  :config
  (better-jumper-mode 1))

(use-package boon
  :ensure t
  :demand t
  :bind
  ("C-c C-r" . madmacs-boon-reset)
  ("C-o" . better-jumper-jump-backward)
  ("C-i" . better-jumper-jump-foreward)
  
  :config
  (boon-mode 1)
  
  (require 'boon-qwerty-hjkl)
  
  (define-key boon-command-map "=" 'indent-according-to-mode)
  (define-key boon-command-map "r" 'swiper)
  (define-key boon-goto-map "l" 'consult-line)

  (global-set-key (kbd "M-o") 'pop-global-mark)

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
