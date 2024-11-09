;; -*- lexical-binding: t; -*-


;;; Eglot
(use-package eglot
  :ensure nil
  :straight nil
  :after project
  :custom
  (eglot-report-progress t)
  (eglot-extend-to-xref t)
  (eglot-autoreconnect 5)

  :config
  (setopt eldoc-echo-area-use-multiline-p 1))


(defun madmacs--lsp (&rest args)
  "If LSP is eglot, call eglot-ensure, otherwise call lsp-deferred."
  (apply #'eglot-ensure args))

(provide 'madmacs-coding-lsp)
