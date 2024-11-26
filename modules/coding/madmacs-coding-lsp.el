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

(use-package consult-xref-stack
  :ensure nil
  :straight (consult-xref-stack :type git :host github :repo "brett-lempereur/consult-xref-stack" :branch "main")
  :bind
  (("C-M-," . consult-xref-stack-forward)
    ("M-," . consult-xref-stack-backward)))

(provide 'madmacs-coding-lsp)
