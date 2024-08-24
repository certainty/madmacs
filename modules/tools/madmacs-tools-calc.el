;; -*- lexical-binding: t; -*-

(use-package calc
  :ensure nil
  :straight nil)

(use-package casual-calc
  :ensure t
  :after calc
  :bind
  (:map calc-mode-map ("M-o" . casual-calc-tmenu))
  (:map calc-alg-map ("M-o". casual-calc-tmenu)))

(provide 'madmacs-tools-calc)
