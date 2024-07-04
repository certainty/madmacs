;; -*- lexical-binding: t; -*-

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

(use-package evil-repeat
  :ensure nil
  :straight nil

  :config
  ;; restore some keybindings stolen by evil
  (evil-define-key '(normal visual) 'global (kbd "C-.") 'embark-act)
  (evil-define-key '(normal visual) 'global (kbd "M-.") 'embark-dwim)

  (global-set-key (kbd  "C-.") 'embark-act)
  (global-set-key (kbd  "M-.") 'embark-dwim))

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

(use-package evil-textobj-tree-sitter
  :ensure t
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj  "function.inner"))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

(provide 'madmacs-keys-evil)

