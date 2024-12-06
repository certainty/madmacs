;; -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :straight nil
  :hook (dired-mode . dired-omit-mode)

  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls")))

(use-package dired-x
  :ensure nil
  :straight nil
  :config
  (setopt dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(use-package dirvish
  :ensure t
  :hook (dirvish-mode . dired-omit-mode)
  :init
  (dirvish-override-dired-mode)

  :custom
  (dirvish-quick-access-entris          ; It's a custom option, `setq' won't work
    '(("h" "~/"                "Home")
       ("d" "~/Downloads/"      "Downloads")
       ("w" "~/NewWork/Code/"   "Work")
       ("p" "~/Private/"        "Private")))

  (dirvish-side-width 100)
  (dirvish-side-auto-close t)

  (dirvish-use-header-line 'global)
  (dirvish-default-layout '(0 0.3 0.7))
  (dirvish-mode-line-format
    '(:left (sort symlink) :right (omit yank index)))

  (dirvish-attributes
    '(subtree-state collapse))

  :bind                                 ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  ("C-x d" . dirvish-side)
  
  (:map dirvish-mode-map                ; Dirvish inherits `dired-mode-map'
    ("a"   . dirvish-quick-access)
    ("f"   . dirvish-file-info-menu)
    ("y"   . dirvish-yank-menu)
    ("N"   . dirvish-narrow)
    ("^"   . dirvish-history-last)
    ("<tab>" . dirvish-toggle-subtree)
    ("h"   . dirvish-history-jump)      ; remapped `describe-mode'
    ("s"   . dirvish-quicksort)         ; remapped `dired-sort-toggle-or-edit'
    ("v"   . dirvish-vc-menu)           ; remapped `dired-view-file'
    ("O"   . dired-omit-mode)
    ("M-f" . dirvish-history-go-forward)
    ("M-b" . dirvish-history-go-backward)
    ("M-l" . dirvish-ls-switches-menu)
    ("M-m" . dirvish-mark-menu)
    ("M-t" . dirvish-layout-toggle)
    ("M-s" . dirvish-setup-menu)
    ("M-j" . dirvish-fd-jump)))

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map
          ("M-o" . casual-dired-tmenu)))

(use-package wdired
  :ensure nil
  :straight nil
  :commands (wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook dired-mode)

(provide 'madmacs-files-dired)
