;; (use-package moody
;;   :config
;;   (moody-replace-mode-line-front-space)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-time nil)
  (doom-modeline-check-simple-format t)

  :config
  (doom-modeline-mode))

(provide 'madmacs-ui-modeline)
