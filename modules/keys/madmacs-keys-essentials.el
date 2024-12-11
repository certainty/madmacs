;; -*- lexical-binding: t; -*-

(use-package bind-key
  :ensure nil
  :straight nil
  :custom
  (bind-key-describe-special-forms nil))

(use-package which-key
  :ensure t
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

  :config
  (which-key-mode))


;; we also setup some keymaps for ourselves and make sure some keys are free
(use-package emacs
  :ensure t
  :straight nil

  :bind

  (:map madmacs-keymap-packages
    ("u" . straight-pull-package)
    ("U" . straight-pull-all))
  
  :init
  ;; make room in the goto-map
  (unbind-key "M-g n") ; notes prefix
  
  (defvar-keymap madmacs-keymap-global :doc "The keymap used for global commands. This is the home of keys that don't live in other global maps like C-x or C-c")
  (defvar-keymap madmacs-keymap-ux :doc "UX settings")
  (defvar-keymap madmacs-keymap-packages :doc "Package functionality")
  (defvar-keymap madmacs-keymap-notes :doc "Note taking functionality")
  (defvar-keymap madmacs-keymap-ai :doc "AI related functionality")
  
  (which-key-add-keymap-based-replacements madmacs-keymap-global
    "i" `("AI" . ,madmacs-keymap-ai)
    "n" `("Notes" . ,madmacs-keymap-notes)
    "p" `("Packages" . ,madmacs-keymap-packages)
    "u" `("UX" . ,madmacs-keymap-ux))

  (which-key-add-keymap-based-replacements madmacs-mode-map
    "C-j" `("Madmacs" . ,madmacs-keymap-global)))
 
(provide 'madmacs-keys-essentials)
