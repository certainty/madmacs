
(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-check-icon nil)
  (doom-modeline-major-mode-icon nil)

  :config
  (doom-modeline-mode 1)
  (setq max-mini-window-height 0.2)
  )

(provide 'madmacs-ui-modeline)
