
(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (catppuccin-set-color 'base "#000000")
  (load-theme 'catppuccin :no-confirm))



;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (setq
;;    modus-themes-custom-auto-reload t
;;    modus-themes-disable-other-themes nil
;;    modus-themes-mixed-fonts t
;;    modus-themes-italic-constructs t
;;    modus-themes-bold-constructs t
;;    modus-themes-variable-pitch-ui t
;;    modus-themes-prompts '(extrabold italic)
;;    modus-themes-completions
;;    '((matches . (extrabold underline))
;;      (selection . (semibold italic))))
;;   (setq modus-themes-common-palette-overrides
;;         (madmacs--defaults-with-overrides modus-themes-preset-overrides-faint
;;                                     `((bg-main "#000000")
;;                                       ;(bg-paren-match bg-magenta-intense)
;;                                       ;(underline-paren-match fg-main)
;;                                       ;(border-mode-line-active unspecified)
;;                                       ;(border-mode-line-inactive unspecified)
;;                                       ;(fg-line-number-active red-cooler)
;;                                       ;(bg-line-number-inactive unspecified)
;;                                       ;(bg-line-number-active unspecified)
;;                                       )))
;;   (load-theme 'modus-vivendi-tinted t))

(provide 'madmacs-ui-theme)
