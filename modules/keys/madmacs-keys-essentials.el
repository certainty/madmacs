(use-package bind-key
  :ensure nil
  :straight nil
  :custom
  (bind-key-describe-special-forms nil))

(use-package which-key
  :ensure t
  :defer 1 ; only if we do this will meow correctly use which-key
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-max-display-columns 8)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay .75)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-setup-minibuffer)
  (which-key-separator " → ")
  :init
  (which-key-mode))

(use-package general
  :ensure t)

(provide 'madmacs-keys-essentials)
