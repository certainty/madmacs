(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)

  :config
  (setopt which-key-allow-evil-operators t)
  (setopt evil-emacs-state-modes nil)
  (setopt evil-insert-state-modes nil)
  (setopt evil-motion-state-modes nil)

  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal "," t)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-surround
  :ensure t)

(use-package evil-indent-plus
  :ensure t)

(use-package expand-region
  :ensure t)

(provide 'madmacs-keys-evil)

