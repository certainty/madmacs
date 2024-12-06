;; -*- lexical-binding: t; -*-


;; TODO: rethink this approach
;; * global keybindings under a certain prefix
;; * M-g is prefix for goto (mode specific overwrites may apply)
;; * M-s is prefix for search
;; * C-c is for mode specific overwrites (place these overwrites in the corresponding mode-maps)
;; * C-c e is for lsp
;; * C-c v is for vc
;; * C-x have replacements with consult where appropriate
;; * Embark is used for contextual information
(use-package emacs
  :ensure nil
  :straight nil
  :demand t
  :config
  (keymap-set madmacs-mode-map "C-j" madmacs-keymap-global))

(provide 'madmacs-keys-keybindings)

