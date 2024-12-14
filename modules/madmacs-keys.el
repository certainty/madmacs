;; -*- lexical-binding: t; -*-

(use-package bind-key
  :straight nil
  :custom
  (bind-key-describe-special-forms nil))

(use-package which-key
  :straight nil
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-max-display-columns 10)
  (which-key-show-early-on-C-h t)
  (which-key-show-prefix nil)
  (which-key-idle-delay .75)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-setup-minibuffer)
  (which-key-separator " • ")
  (which-key-prefix-prefix nil)

  :init
  (which-key-mode))

(use-package emacs
  :straight nil
  :bind
  (:map madmacs-mode-map
    ("C-z" . repeat) ; I don't use suspend frame so this is a nicer binding than C-x z
    ("C-x z" . repeat-complext-command)) ; now this can go here
  
  (:map madmacs-keymap-packages
    ("u" . straight-pull-package)
    ("U" . straight-pull-all))
  
  :init
  (unbind-key "M-g n")
  
  (defvar-keymap madmacs-keymap-global :doc "The keymap used for global commands. This is the home of keys that don't live in other global maps like C-x or C-c")
  (defvar-keymap madmacs-keymap-ux :doc "UX settings")
  (defvar-keymap madmacs-keymap-packages :doc "Package functionality")
  (defvar-keymap madmacs-keymap-notes :doc "Note taking functionality")
  (defvar-keymap madmacs-keymap-ai :doc "AI related functionality")
  
  (which-key-add-keymap-based-replacements madmacs-keymap-global
    "l" `("AI" . ,madmacs-keymap-ai)
    "n" `("Notes" . ,madmacs-keymap-notes)
    "p" `("Packages" . ,madmacs-keymap-packages)
    "u" `("UX" . ,madmacs-keymap-ux))

  (which-key-add-keymap-based-replacements madmacs-mode-map
    "C-j" `("Madmacs" . ,madmacs-keymap-global)))

(provide 'madmacs-keys)
