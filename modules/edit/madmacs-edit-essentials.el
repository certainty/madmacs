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
  :ensure t)

(use-package symbol-overlay
  :ensure t)

;
(use-package casual-symbol-overlay
  :ensure t
  :straight (casual-symbol-overlay :type git :host github :repo "kickingvegas/casual-symbol-overlay")
  :after symbol-overlay
  :bind (:map symbol-overlay-map ("M-o" . casual-symbol-overlay-tmenu)))

(provide 'madmacs-edit-essentials)
