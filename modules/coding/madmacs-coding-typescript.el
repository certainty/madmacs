
(use-package jest-test-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . jest-test-mode)
  (typescript-mode . lsp-deferred)
  (js-mode . lsp-deferred))

(use-package prettier-js
  :ensure t
  :after typescript-mode
  :hook typescript-mode)

(provide 'madmacs-coding-typescript)
