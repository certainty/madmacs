;; -*- lexical-binding: t; -*-

;;; Faces

(use-package highlight-numbers
  :ensure t
  :hook prog-mode)

(use-package hl-todo
  :ensure t
  :hook prog-mode
  :bind
  (:map madmacs-keymap-ux
    ("t" . hl-todo-mode)))

(use-package goggles
  :ensure t
  :hook (prog-mode text-mode)
  :custom
  (goggles-pulse nil))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package reveal
  :ensure t
  :straight nil
  :custom
  (reveal-auto-hide nil)
  :config
  (global-reveal-mode))

(use-package emacs
  :ensure t
  :demand t
  :straight nil
  :bind
  (:map madmacs-keymap-ux
    ("l" . display-line-numbers-mode)
    ("h" . global-hl-line-mode))
  
  :custom
  (indicate-empty-lines nil))

;;; Fonts

(use-package fontaine
  :ensure t
  :demand t
  :bind
  (:map madmacs-keymap-ux
    ("f" . madmacs-toggle-font))
  
  :config
  (defun madmacs-toggle-font ()
    (interactive)
    (if (eq fontaine-current-preset 'regular)
      (fontaine-set-preset 'presentation)
      (fontaine-set-preset 'regular)))

  (fontaine-mode)
  (diminish 'fontaine-mode)
  (fontaine-set-preset 'regular)
  
  :custom
  (x-underline-at-descent-line nil)
  (fontaine-presets
    `((regular)
       (presentation
         :default-height ,madmacs-presentation-font-size)
       (presentation-bigger
         :default-height ,madmacs-presentation-bigger-font-size)
       (t
         :default-family ,madmacs-default-font
         :default-weight regular
         :default-slant normal
         :default-height ,madmacs-default-font-size

         :fixed-pitch-family ,madmacs-default-font
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family ,madmacs-variable-pitch-font
         :variable-pitch-height 1.0))))


(use-package nerd-icons
  :ensure t)

;;; Theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind
  (:map madmacs-keymap-ux
    ("c" . madmacs-modus-theme-toggle))
  
  :config
  (setq
    modus-themes-mode-line '(moody)
    modus-themes-custom-auto-reload t
    modus-themes-disable-other-themes nil
    modus-themes-mixed-fonts t
    modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-variable-pitch-ui t
    modus-themes-prompts '(extrabold italic)
    modus-themes-paren-match '(bold intense)
    modus-themes-org-blocks 'tinted-background
    modus-themes-completions
    '((matches . (extrabold underline))
       (selection . (semibold italic))))

  (defun madmacs-load-modus-theme (name &optional light)
    (setq modus-themes-common-palette-overrides
      (madmacs--defaults-with-overrides modus-themes-preset-overrides-faint
        `(,(unless light '(bg-main "#000000"))
           ,(unless light '(bg-paren-match bg-magenta-intense))
           (underline-paren-match fg-main)
           (border-mode-line-active unspecified)
           (border-mode-line-inactive unspecified)
           (fg-line-number-active red-cooler)
           (bg-line-number-inactive unspecified)
           (bg-line-number-active unspecified))))

    (load-theme name t))

	(defvar madmacs--modus-theme-light nil)

  (defun madmacs-modus-light-theme ()
    (interactive)
    (setq madmacs--modus-theme-light t)
    (madmacs-load-modus-theme 'modus-operandi-tinted t))

  (defun madmacs-modus-dark-theme ()
    (interactive)
    (setq madmacs--modus-theme-light nil)
    (madmacs-load-modus-theme 'modus-vivendi-tinted))

  (defun madmacs-modus-theme-toggle ()
    (interactive)
    (if madmacs--modus-theme-light
      (madmacs-modus-dark-theme)
      (madmacs-modus-light-theme)))
  (madmacs-modus-dark-theme))

(provide 'madmacs-ui-look)
