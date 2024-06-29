(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(provide 'madmacs-ui-modeline)
