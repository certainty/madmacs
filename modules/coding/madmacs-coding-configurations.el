;; -*- lexical-binding: t; -*-

(use-package yaml
  :ensure t)

(use-package yaml-mode
  :ensure nil
  :straight nil
  :hook
  (yaml-mode  . madmacs--lsp))

(use-package json-mode
  :ensure nil
  :straight nil
  :hook
  (json-mode . madmacs--lsp))


(provide 'madmacs-coding-configurations)
