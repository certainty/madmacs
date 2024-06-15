(use-package fontaine
  :ensure t
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


(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 1)

  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts 'yes)))

(use-package all-the-icons-dired
  :ensure t
  :hook dired-mode)

(use-package all-the-icons-completion
  :ensure t
  :straight (all-the-icons-completion :type git :host github :repo "MintSoup/all-the-icons-completion")
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  (emacs-startup . all-the-icons-completion-mode))

(provide 'madmacs-ui-fonts)