(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))


(use-package go-guru
  :ensure t)

(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup))

(provide 'madmacs-coding-golang)
