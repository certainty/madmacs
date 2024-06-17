(use-package yaml
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook
  (yaml-mode  . lsp-deferred))

(use-package json-mode
  :ensure t
  :hook (json-mode . lsp-deferred)
  :mode (("\\.json\\'" . json-mode)))

(use-package json-snatcher
  :ensure t)

(provide 'madmacs-coding-configuration)
