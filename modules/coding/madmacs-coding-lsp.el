;; -*- lexical-binding: t; -*-

;;; Eglot
(use-package eglot
  :ensure nil
  :straight nil
  :after (project embark)
  :custom
  (eglot-report-progress t)
  (eglot-extend-to-xref t)
  (eglot-autoreconnect 5)

  :bind
  ((:map embark-identifier-map
     ("^" . helpful-at-point)
     ("l r" . eglot-rename)
     ("l i" . eglot-find-implementation)
     ("l d" . eglot-find-declaration)))

  :config
  (setopt eldoc-echo-area-use-multiline-p 1)
  (with-eval-after-load 'embark
    (push 'embark--allow-edit
      (alist-get 'eglot-rename embark-target-injection-hooks))))

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
