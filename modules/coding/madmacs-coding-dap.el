
(use-package dap-mode
  :ensure t
  :bind
  (:map madmacs-debugger-keys
        ("d" . dap-hydra))
  :hook (dap-mode . dap-tooltip-mode))

(use-package dap-ui
  :ensure nil
  :straight nil
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

(use-package posframe
  :ensure t)

(provide 'madmacs-coding-dap)
