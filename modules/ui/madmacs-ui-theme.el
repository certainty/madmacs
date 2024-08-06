;; -*- lexical-binding: t; -*-

(use-package catppuccin-theme
  :ensure t
  :straight (catppuccin-theme :type git :host github :repo "catppuccin/emacs")
  :custom
  (catppuccin-flavor 'macchiato)
  (catppuccin-highlight-matches t)
  (catppuccin-italic-comments t)
  (catppuccin-italic-variables t)

  :config
  ;; I like it extra dark
  (catppuccin-set-color 'base "#000000")
  (custom-set-faces `(corfu-border ((t (:background ,(catppuccin-get-color 'mauve) :foreground ,(catppuccin-get-color 'mauve))))))

  (load-theme 'catppuccin :no-confirm))

;; Modus is nice too - so enable it if you're bored of catppuccin-theme
;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (setq
;;    modus-themes-mode-line '(moody)
;;    modus-themes-custom-auto-reload t
;;    modus-themes-disable-other-themes nil
;;    modus-themes-mixed-fonts t
;;    modus-themes-italic-constructs t
;;    modus-themes-bold-constructs t
;;    modus-themes-variable-pitch-ui t
;;    modus-themes-prompts '(extrabold italic)
;;    modus-themes-paren-match '(bold intense)
;;    modus-themes-org-blocks 'tinted-background
;;    modus-themes-completions
;;    '((matches . (extrabold underline))
;;      (selection . (semibold italic))))
;;   (setq modus-themes-common-palette-overrides
;;         (madmacs--defaults-with-overrides modus-themes-preset-overrides-faint
;;                                     `((bg-main "#000000")
;;                                       (bg-paren-match bg-magenta-intense)
;;                                       (underline-paren-match fg-main)
;;                                       (border-mode-line-active unspecified)
;;                                       (border-mode-line-inactive unspecified)
;;                                       (fg-line-number-active red-cooler)
;;                                       (bg-line-number-inactive unspecified)
;;                                       (bg-line-number-active unspecified)
;;                                       )))
;;   (load-theme 'modus-vivendi-tinted t))

(provide 'madmacs-ui-theme)
