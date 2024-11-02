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

(provide 'madmacs-keys-essentials)
