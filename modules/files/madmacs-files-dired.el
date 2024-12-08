;; -*- lexical-binding: t; -*-
(use-package dired
  :ensure nil
  :straight nil
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)

  :bind
  (:map dired-jump-map
    ("j" . nil))
  
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (dired-make-directory-clickable t) 
  (dired-free-space nil) 
  (dired-mouse-drag-files t) 


  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls")))

(use-package dired-aux
  :ensure nil
  :straight nil
  :after dired
  :bind
  (:map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ("M-s f" . nil)
    ("C-<return>" . dired-do-open)
    ("C-x v v" . dired-vc-next-action))
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask) 
  (dired-vc-rename-file t)             
  (dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) 
  (dired-create-destination-dirs-on-trailing-dirsep t))

(use-package dired-x
  :ensure nil
  :straight nil
  :ensure nil
  :after dired
  :bind
  (:map dired-mode-map
    ("I" . dired-info))

  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-x-hands-off-my-keys t)
  (dired-bind-man nil)
  (dired-bind-info nil))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

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
