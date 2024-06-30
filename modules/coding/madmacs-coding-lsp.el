(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-completion-mode)
  (lsp-mode . lsp-enable-which-key-integration)

  :custom
  (lsp-keymap-prefix "<leader>l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-auto-configure t)
  (lsp-auto-execute-action nil)
  (lsp-lens-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-semgrep-languages nil)
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 3000))


(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ("<leader>l !" . consult-lsp-diagnostics)
        ("<leader>l S" . consult-lsp-file-symbols))
  :config
  ;; TODO: add LSP replacements for which-keys
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'madmacs-coding-lsp)
