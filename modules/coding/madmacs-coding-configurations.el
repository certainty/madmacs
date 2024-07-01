(use-package yaml
  :ensure t)

(use-package yaml-ts-mode
  :ensure nil
  :straight nil
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook
  (yaml-ts-mode  . lsp-deferred))


(use-package json-ts-mode
  :ensure nil
  :straight nil
  :hook (json-ts-mode . lsp-deferred)
  :mode (("\\.json\\'" . json-ts-mode)))


(provide 'madmacs-coding-configurations)
