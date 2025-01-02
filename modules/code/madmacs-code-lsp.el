;; -*- lexical-binding: t; -*-

;;; Eglot
(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-report-progress t)
  (eglot-extend-to-xref t)
  (eglot-autoreconnect 5)
  (eldoc-echo-area-use-multiline-p 1)

  :bind
  (:map madmacs-keymap-eglot
    ("r" . eglot-reconnect)
    ("q" . eglot-shutdown)
    ("l"  . eglot-shutdown-all)
    ("c" . eglot-show-workspace-configuration)
    ("a" . eglot-code-actions)
    ("F" . eglot-code-action-quickfix)
    ("u h" . eglot-inlay-hints-mode)
    ("f b" . eglot-format-buffer)
    ("f f" . eglot-format))
   
  (:map madmacs-keymap-eglot
    ("M-g e a" . xref-find-apropos)
    ("M-g e d" . xref-find-definitions)
    ("M-g e r" . eglot-rename)
    ("M-g e i" . eglot-find-implementation)
    ("M-g e D" . eglot-find-declaration)
    ("M-g e t" . eglot-find-typeDefinition))

  (:map madmacs-keymap-eglot-embark
    ("^" . helpful-at-point)
    ("e d" . xref-find-definitions)
    ("e r r" . eglot-rename)
    ("e a" . eglot-code-actions)
    ("e r i" . eglot-code-action-inline)
    ("e i" . eglot-find-implementation)
    ("e D" . eglot-find-declaration)
    ("e t" . eglot-find-typeDefinition))

  :init
  (defvar-keymap madmacs-keymap-eglot :doc "LSP eglot map")
  (defvar-keymap madmacs-keymap-eglot-embark :doc "Embark map for eglot")

  (with-eval-after-load 'embark
    (keymap-set embark-identifier-map "e" madmacs-keymap-eglot-embark)
    
    (push 'embark--allow-edit
      (alist-get 'eglot-rename embark-target-injection-hooks))
    
    (push 'embark--allow-edit
      (alist-get 'eglot-code-actions embark-target-injection-hooks)))

  :config
  (which-key-add-keymap-based-replacements eglot-mode-map
    "C-c e" `("LSP…" . ,madmacs-keymap-eglot)))

(provide 'madmacs-code-lsp)
