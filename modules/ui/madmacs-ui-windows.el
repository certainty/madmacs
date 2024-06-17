(use-package window
  :straight nil
  :ensure nil
  :custom
  (display-buffer-base-action nil))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  (:map madmacs-toggle-map
        ("W" . toggle-frame-fullscreen))
  (:map madmacs-window-map
        ("h" . windmove-left)
        ("j" . windmove-down)
        ("k" . windmove-up)
        ("l" . windmove-right)
        ("a" . ace-select-window)
        ("b" . balance-windows)
        ("v" . split-window-right)
        ("H" . split-window-below)
        ("o" . other-window)
        ("w" . ace-window)
        ("q" . delete-window)
        ("d" . delete-window)
        ("x" . ace-swap-window)
        ("m" . ace-maximize-window)))

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

(provide 'madmacs-ui-windows)
