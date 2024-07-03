;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
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
  (lsp-file-watch-threshold 3000)

  :config
  (lsp-enable-which-key-integration))

(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :config
  ;; TODO: add LSP replacements for which-keys
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;;; Eglot
(use-package eglot
  :ensure nil
  :straight nil
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-stay-out-of 'eldoc))

(defun madmacs--lsp (&rest args)
  "If LSP is eglot, call eglot-ensure, otherwise call lsp-deferred."
  (if (eq madmacs-lsp-client 'eglot)
      (apply #'eglot-ensure args)
    (apply #'lsp-deferred args)))

(provide 'madmacs-coding-lsp)
