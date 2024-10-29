;; -*- lexical-binding: t; -*-

(use-package yaml
  :ensure t)

(use-package yaml-mode
  :ensure nil
  :straight nil
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode  . madmacs--lsp))

(use-package json-mode
  :ensure nil
  :straight nil
  :mode "\\.json\\'"
  :hook
  (json-mode . madmacs--lsp))

(use-package csv-mode
  :defer t
  :hook (csv-mode . csv-guess-set-separator) 
  :mode "\\.csv\\'")

(provide 'madmacs-coding-configurations)
