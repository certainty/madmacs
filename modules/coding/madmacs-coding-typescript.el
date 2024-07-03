;; -*- lexical-binding: t; -*-

(use-package jest-test-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . jest-test-mode)
  (typescript-mode . madmacs--lsp)
  (typescript-ts-mode . madmacs--lsp)
  (typescript-ts-mode . jest-test-mode)
  (js-mode . madmacs--lsp)
  (js-ts-mode . madmacs--lsp))

(use-package prettier-js
  :ensure t
  :hook (typescript-mode typescrip-ts-mode))

(provide 'madmacs-coding-typescript)
