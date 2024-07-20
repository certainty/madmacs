;; -*- lexical-binding: t; -*-

(use-package dap-mode
  :disabled t ; we use eglot
  :ensure t
  :hook (dap-mode . dap-tooltip-mode))

(use-package dap-ui
  :disabled t ; we use eglot
  :ensure nil
  :straight nil
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

;; TODO: does it have any consequences if I don't load this? 
;; (use-package posframe
;;   :ensure t)

;; (use-package realgud
;;   :ensure t)

(use-package dape
  :ensure t)

(provide 'madmacs-coding-debug)
