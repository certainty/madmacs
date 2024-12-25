;; -*- lexical-binding: t; -*-


;;; Foundational configuration of emacs display elements
(use-package emacs
  :custom
  (frame-inhibit-implied-resize nil)
  (frame-title-format "\n")
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-splash-screen t)
  (inhibit-startup-echo-area-message "david.krentzlin")
  (initial-scratch-message nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (initial-major-mode 'fundamental-mode))

(use-package diminish
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fontaine
  :demand t
  :after diminish
  :bind
  (:map madmacs-keymap-ux
	("f" . madmacs-toggle-font))

  :custom
  (x-underline-at-descent-line nil)
  (fontaine-presets
   `((regular)
     (presentation
      :default-height ,madmacs-presentation-font-size)
     (presentation-bigger
      :default-height ,madmacs-presentation-bigger-font-size)
     (t
      :default-family ,madmacs-font
      :default-weight regular
      :default-slant normal
      :default-height ,madmacs-font-size

      :fixed-pitch-family ,madmacs-font
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-height 1.0

      :variable-pitch-family ,madmacs-variable-pitch-font
      :variable-pitch-height 1.0)))

  :config
  (defun madmacs-toggle-font ()
    (interactive)
    (if (eq fontaine-current-preset 'regular)
      (fontaine-set-preset 'presentation)
      (fontaine-set-preset 'regular)))

  (fontaine-mode 1)
  (message "setting font preset")
  (fontaine-set-preset 'regular)
  (diminish 'fontaine-mode))

;;; Icons

(use-package nerd-icons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modus-themes
  :demand t
  :bind
  (:map madmacs-keymap-ux
	("c" . madmacs-modus-theme-toggle))

  :config
  (setq
   modus-themes-custom-auto-reload t
   modus-themes-disable-other-themes nil
   modus-themes-mixed-fonts t
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-variable-pitch-ui t
   modus-themes-prompts '(extrabold italic)
   modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold italic))))

  (defun madmacs--defaults-with-overrides (defaults overrides)
    "Merge overrides into defaults. Overriding any defaults."
    (append (cl-remove-if (lambda (x) (member (car x) (mapcar 'car overrides))) defaults) overrides))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package moody
  :demand t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :demand t
  :config
  (minions-mode))


(provide 'madmacs-base-ui)
