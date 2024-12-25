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
	  ("c" . modus-themes-toggle))

  :config
  (setq
    modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi)
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
  
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package ef-themes
  :custom
  (ef-themes-to-toggle '(ef-light ef-dream)))

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
