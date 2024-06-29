(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (setopt which-key-allow-evil-operators t)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'insert (kbd "C-SPC"))
  (evil-set-leader 'normal "," t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-surround
  :ensure t)

(use-package evil-indent-plus
  :ensure t)

(use-package neotree
  :ensure t
  :custom
  (neo-theme 'icons))

(provide 'madmacs-keys-evil)

