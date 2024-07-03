;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :hook (web-mode . madmacs--lsp))

(use-package css-mode
  :ensure t
  :hook
  (css-mode . madmacs--lsp)
  (css-ts-mode . madmacs--lsp))

(provide 'madmacs-coding-web)
