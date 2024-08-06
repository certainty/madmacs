;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :straight nil
  :custom
  :bind
  ;; This is what I usually want
  ("M-z" . zap-up-to-char))

(use-package expand-region
  :ensure t)

;; expand region using treesitter
(use-package expreg
  :ensure t)

(provide 'madmacs-edit-essentials)
