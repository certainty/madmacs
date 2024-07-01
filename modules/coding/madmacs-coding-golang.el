;; (use-package go-mode
;;   :ensure t
;;   :mode "\\.go\\'"
;;   :hook
;;   (go-mode . lsp-deferred)
;;   (before-save . gofmt-before-save)
;;   :config
;;   (setq gofmt-command "goimports"))

(use-package go-ts-mode
  :ensure nil
  :straight nil
  :mode "\\.go\\'"
  :hook
  (go-ts-mode . lsp-deferred))

(use-package go-mod-ts-mode
  :ensure nil
  :straight nil
  :mode "\\.mod\\'"
  :hook
  (go-mod-ts-mode . lsp-deferred))

(use-package go-guru
   :ensure t)

(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
  (go-ts-mode . flycheck-golangci-lint-setup))

(provide 'madmacs-coding-golang)
