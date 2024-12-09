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
  ((:map madmacs-keymap-eglot
     ("r" . eglot-reconnect)
     ("q" . eglot-shutdown)
     ("l"  . eglot-shutdown-all)
     ("c" . eglot-show-workspace-configuration)
     ("a" . eglot-code-actions)
     ("F" . eglot-code-action-quickfix)
     ("u h" . eglot-inlay-hints-mode)
     ("f b" . eglot-format-buffer)
     ("f f" . eglot-format))
    (:map eglot-mode-map
      ("M-g e a" . xref-find-apropose)
      ("M-g e d" . xref-find-definitions)
      ("M-g e r" . eglot-rename)
      ("M-g e i" . eglot-find-implementation)
      ("M-g e D" . eglot-find-declaration)
      ("M-g e t" . eglot-find-typeDefinition))

    ;; enable actions at point 
    (:map embark-identifier-map
      ("^" . helpful-at-point)
      ("e d" . xref-find-definitions)
      ("e r r" . eglot-rename)
      ("e a" . eglot-code-actions)
      ("e r i" . eglot-code-action-inline)
      ("e i" . eglot-find-implementation)
      ("e D" . eglot-find-declaration)
      ("e t" . eglot-find-typeDefinition))
    (:map embark-general-map
      ("e a" . eglot-code-actions)))

  :init
  (defvar-keymap madmacs-keymap-eglot :doc "LSP eglot map")


  :config
  (which-key-add-keymap-based-replacements eglot-mode-map
    "C-c e" `("LSP…" . ,madmacs-keymap-eglot))
  
  (setopt eldoc-echo-area-use-multiline-p 1)
  (with-eval-after-load 'embark
    (push 'embark--allow-edit
      (alist-get 'eglot-rename embark-target-injection-hooks))
    (push 'embark--allow-edit
      (alist-get 'eglot-code-actions embark-target-injection-hooks))))

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
