(use-package lsp-mode
  :ensure t
  :bind
  (:map madmacs-goto-map
        ("d" . etags-xref-find-definitions-tag-order)
        ("b" . xref-go-back)
        ("R" . xref-find-references))

  :hook
  (lsp-mode . lsp-completion-mode)
  (lsp-mode . lsp-enable-which-key-integration)

  :custom
  (lsp-keymap-prefix "C-c l")
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
        ("C-c l !" . consult-lsp-diagnostics)
        ("C-c l S" . consult-lsp-file-symbols))
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'madmacs-coding-lsp)
