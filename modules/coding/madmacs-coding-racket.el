;; -*- lexical-binding: t; -*-

(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :custom
  (racket-program "/opt/homebrew/bin/racket")
  :config
  ;; we go with lispy mode here
  (define-key racket-mode-map "(" nil)
  (define-key racket-mode-map "{" nil)
  (define-key racket-mode-map "[" nil)

  (defvar-keymap madmacs-racket-expand-keys :doc "Racket expand keys.")
  (which-key-add-keymap-based-replacements madmacs-racket-expand-keys
    "e" `("expand sexp" . racket-expand-last-sexp)
    "d" `("expand definition" . racket-expand-definition)
    "r" `("expand region" . racket-expand-region)
    "f" '("expand file" . racket-expand-file))

  (defvar-keymap madmacs-racket-send-keys :doc "Racket send keys.")
  (which-key-add-keymap-based-replacements madmacs-racket-send-keys
    "d" `("send definition" . racket-send-definition)
    "e" `("send expression" . racket-send-last-sexp)
    "r" `("send region" . racket-send-region)
    "f" '("send file" . racket-send-file))
    
  (which-key-add-keymap-based-replacements racket-mode-map
    "C-c t" '("test" . racket-test)
    "C-c e" `("expand" . ,madmacs-racket-expand-keys)
    "C-c s" `("send" . ,madmacs-racket-send-keys)
    "C-c d" '("documentation" . racket-documentation-search)
    "C-c l" `("logger" . racket-logger)
    "C-c o" `("profile" . racket-profile)
    "C-c z" `("repl" . racket-edit-switch-to-repl)))
  
  (provide 'madmacs-coding-racket)
