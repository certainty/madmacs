
(use-package dap-mode
  :ensure t
  :hook (dap-mode . dap-tooltip-mode))

(use-package dap-ui
  :ensure nil
  :straight nil
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

(use-package posframe
  :ensure t)

;; (use-package realgud
;;   :ensure t)

(provide 'madmacs-coding-debug)
