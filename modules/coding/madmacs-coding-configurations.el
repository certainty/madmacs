;; -*- lexical-binding: t; -*-

(use-package yaml
  :ensure t)

(use-package yaml-mode
  :ensure nil
  :straight nil
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook
  (yaml-ts-mode  . madmacs--lsp)
  (yaml-mode  . madmacs--lsp))


(use-package json-mode
  :ensure nil
  :straight nil
  :hook
  (json-ts-mode . madmacs--lsp)
  (json-mode . madmacs--lsp)
  :mode (("\\.json\\'" . json-mode)))


(provide 'madmacs-coding-configurations)
