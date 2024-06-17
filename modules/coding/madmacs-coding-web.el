(use-package web-mode
  :ensure t
  :hook (web-mode . lsp-deferred))

(use-package css-mode
  :ensure t
  :hook (css-mode . lsp-deferred))

(provide 'madmacs-coding-web)
