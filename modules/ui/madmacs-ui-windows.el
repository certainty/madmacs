(use-package window
  :straight nil
  :ensure nil
  :custom
  (display-buffer-base-action nil))

(use-package ace-window
  :ensure t
  :demand t
  :bind
  ("M-o" . ace-window)

  :config
  (which-key-add-keymap-based-replacements madmacs-ux-keys
                                           "W" '("Toggle Fullscreen" . toggle-frame-fullscreen))


  (which-key-add-keymap-based-replacements madmacs-buffers-keys
       "=" '("Balance" . balance-windows)
       "v" '("Split vertical" . split-window-right)
       "H" '("Split horizontal" . split-window-below)
       "o" '("Other" . other-window)
       "b" '("ACE"  . ace-window)
       "d" '("Delete" . delete-window)
       "x" '("Swap" . ace-swap-window)
       "m" '("Maximize" . ace-maximize-window))
  )

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
