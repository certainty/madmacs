;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :straight nil
  :bind
  ;; This is what I usually want
  ("M-z" . zap-up-to-char))

(use-package expand-region
  :ensure t)

;; expand region using treesitter
(use-package expreg
  :ensure t
  :bind
  (:repeat-map madmacs-expreg-repeat-map
    (">" . expreg-expand)
    ("<" . expreg-contract)))

(use-package symbol-overlay
  :ensure t)

(use-package casual-symbol-overlay
  :ensure t
  :straight (casual-symbol-overlay :type git :host github :repo "kickingvegas/casual-symbol-overlay")
  :after symbol-overlay
  :bind (:map symbol-overlay-map ("M-o" . casual-symbol-overlay-tmenu)))

(use-package simple
  :ensure nil
  :straight nil
  :init
  (setopt kill-whole-line t))

(provide 'madmacs-edit-essentials)
