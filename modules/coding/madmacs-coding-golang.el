;; -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure nil
  :straight nil
  :mode "\\.go\\'"
  :hook
  (go-ts-mode . madmacs--lsp)
  (go-mode . madmacs--lsp)
  :config
  (setopt gofmt-command "goimports"))

(use-package go-mod-mode
  :ensure nil
  :straight nil
  :mode "\\.mod\\'"
  :hook
  (go-mod-ts-mode . madmacs--lsp)
  (go-mod-mode . madmacs--lsp))

(use-package go-guru
   :ensure t)

(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
  (go-ts-mode . flycheck-golangci-lint-setup))

(provide 'madmacs-coding-golang)
