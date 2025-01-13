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

;; Modus
(use-package modus-themes
  :if t
  :demand t
  :bind
  (:map madmacs-keymap-ux
	  ("c" . modus-themes-toggle))

  :custom
  (modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (modus-themes-mode-line '(moody))
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-prompts '(extrabold italic))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-completions
    '((matches . (extrabold underline))
       (selection . (semibold italic))))

  :config
  (defun madmacs--defaults-with-overrides (defaults overrides)
    "Merge overrides into defaults. Overriding any defaults."
    (append (cl-remove-if (lambda (x) (member (car x) (mapcar 'car overrides))) defaults) overrides))

  ;; I prefer a less vibrant version of the theme (less greenish)
  (setopt modus-themes-common-palette-overrides
    (madmacs--defaults-with-overrides modus-themes-preset-overrides-faint
      `((bg-paren-match bg-magenta-intense)
         (underline-paren-match fg-main)
         (fg-line-number-active red-cooler))))

  (setopt modus-themes-headings
    '((1 . (variable-pitch 1.1))
       (t . (1.0))))

  (defun madmacs-modus-meow-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
        `(meow-region-cursor-1 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
        `(meow-region-cursor-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
        `(meow-region-cursor-3 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))

        `(meow-position-highlight-number-1 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
        `(meow-position-highlight-number-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
        `(meow-position-highlight-number-3 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-2))))))

  (add-hook 'enable-theme-functions #'madmacs-modus-meow-faces)
  
  ;; Loaded via circadian based on runrise / unset
  ;; (load-theme 'modus-vivendi :no-confirm)
  )

(use-package ef-themes
  :if nil
  :demand t
  :bind
  (:map madmacs-keymap-ux
	  ("c" . ef-themes-toggle))
  :custom
  (ef-themes-toggle '(ef-kassio ef-owl))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-disable-other-themes t)
  (ef-owl-palette-overrides
    '((bg-main "#000000")))
  (ef-dream-palette-overrides
    '((bg-main "#000000")))
  
  :config
  (load-theme 'ef-owl :no-confirm))

(use-package zenburn-theme
  :if nil
  :demand t
  :config
  (load-theme 'zenburn :no-confirm))

;; Toggle Themes based on sunrise / sunset
(use-package solar
  :demand t
  :straight (:type built-in)
  :custom
  (calendar-latitude 53.7)
  (calendar-longitude 10.0166))

(use-package circadian
  :after solar
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
                       (:sunset  . modus-vivendi)))
  :init
  (circadian-setup))

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
