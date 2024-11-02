    
;; -*- lexical-binding: t; -*-
(use-package multiple-cursors
  :ensure t)

(use-package boon
  :ensure t
  :config
  (boon-mode 1)
  (require 'boon-qwerty-hjkl)
  
  (define-key boon-command-map "=" 'indent-according-to-mode)
  (define-key boon-goto-map "l" 'consult-line)
  
  (global-set-key (kbd "M-o") 'pop-global-mark)
  
  (with-eval-after-load 'vterm
    (add-hook 'vterm-mode-hook
      (lambda ()
        (turn-off-boon-mode)))))

(provide 'madmacs-keys-boon)
