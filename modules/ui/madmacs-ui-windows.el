;; -*- lexical-binding: t; -*-

(use-package window
  :straight nil
  :ensure nil
  :custom
  (display-buffer-base-action nil))

(use-package ace-window
  :ensure t
  :demand t)

(use-package windmove
  :straight nil
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package winner
  :straight nil
  :ensure nil
  :config
  (winner-mode 1))

(use-package transpose-frame
  :ensure t)

(use-package popper
  :ensure t
  :bind (("C-`"  . popper-toggle)
          ("M-`"  . popper-cycle)
          ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       help-mode
       compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))



(provide 'madmacs-ui-windows)
