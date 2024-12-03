;; -*- lexical-binding: t; -*-

(define-minor-mode madmacs-mode
  "Minor mode for madmacs, which gives a home for my customizations"
  :global t
  :lighter " #"
  :keymap (make-sparse-keymap)
  )

(add-hook 'find-file-hook 'madmacs-mode)

(provide 'madmacs-ux-minor-mode)
