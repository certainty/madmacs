;; -*- lexical-binding: t; -*-

(use-package files
  :straight (:type built-in)
  :custom
  (large-file-warning-threshold nil)
  (find-file-visit-truename t)

  (make-backup-files nil) ; no backup files
  (vc-make-backup-files nil) ; no backup files in vc
  (create-lockfiles nil) ; no lockfiles since I only ever have one instance of emacs running
  (delete-auto-save-files t)
  (auto-save-default t)
  (auto-save-files-dir "~/.local/cache/emacs")
  (auto-save-list-file-prefix
    (concat "~/.local/cache/auto-save-list" "/.saves-")))

(use-package recentf
  :straight (:type built-in)
  :hook after-init
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never))

(use-package trashed
  :commands (trashed)
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :straight (:type built-in)
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

(use-package casual-suite
  :demand t
  :bind
  (:map bookmark-bmenu-mode-map
    ("C-c m" . casual-bookmark-tmenu))
  (:map dired-mode-map
    ("C-c m" . casual-dired-tmenu)))

(use-package dired-aux
  :straight (:type built-in)
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
  :straight (:type built-in)
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
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-cycle)))

(use-package wdired
  :straight (:type built-in)
  :commands (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
    ("C-x C-q" . wdired-change-to-wdired-mode))
    
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook dired-mode)


(provide 'madmacs-base-files)
