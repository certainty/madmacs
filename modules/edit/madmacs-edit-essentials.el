;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :straight nil
  :init
  ;; This is what I usually want
  ("M-z" . zap-up-to-char)
  (:repeat-map madmacs-zap-repeat-map
    ("z" . zap-up-to-char)
    ("Z" . zap-to-char)))

(use-package expand-region
  :ensure t)

;; expand region using treesitter
(use-package expreg
  :ensure t
  :bind
  ("C->" . expreg-expand)
  ("C-<" . expreg-contract)

  (:repeat-map madmacs-expreg-repeat-map
    (">" . expreg-expand)
    ("<" . expreg-contract)))

(use-package symbol-overlay
  :ensure t)

;
(use-package casual-symbol-overlay
  :ensure t
  :straight (casual-symbol-overlay :type git :host github :repo "kickingvegas/casual-symbol-overlay")
  :after symbol-overlay
  :bind (:map symbol-overlay-map ("M-o" . casual-symbol-overlay-tmenu)))

(provide 'madmacs-edit-essentials)
