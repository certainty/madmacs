;; -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-follow-mode t)
  (treemacs-width 80)
  (treemacs-toggle-width 120)
  (treemacs-workspace-switch-cleanup t)
  (treemacs-peek-mode nil)
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "\\.bloop" "\\.metals" "\\.idea" "\\.ruby-lsp" "target" "\\.bsp"))

  :init
  (defun madmacs--treemacs-ignore-files (names)
    "Ignore files in treemacs with NAMES. NAMES is a list of strings."
    (let ((file-regex (regexp-opt names)))
      (add-to-list 'treemacs-ignored-file-predicates
                   (lambda (file _)
                     (string-match-p file-regex file))))))

(use-package treemacs-evil
  :ensure t
  :if (eql madmacs-modal-approach 'evil)
  :after (treemacs evil))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

 (use-package treemacs-tab-bar
  :ensure t
  :after treemacs
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'madmacs-files-treemacs)
