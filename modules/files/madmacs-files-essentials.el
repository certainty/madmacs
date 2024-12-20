;; -*- lexical-binding: t; -*-

(use-package files
  :ensure nil
  :straight nil
  :custom
  (large-file-warning-threshold nil)
  (find-file-visit-truename t)

  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (vc-make-backup-files t)
  (backup-directory-alist
     `(("." . ,(madmacs--ensure-cache-dir "backups"))))

  (create-lockfiles nil) ; no lockfiles since I only ever have one instance of emacs running
  (delete-auto-save-files t)
  (auto-save-default nil)
  (auto-save-files-dir (madmacs--ensure-cache-dir "auto-save"))
  (auto-save-list-file-prefix
   (concat (madmacs--ensure-cache-dir "auto-save-list") "/.saves-")))

(use-package savehist
  :ensure nil
  :straight nil
  :custom
  (savehist-file  (expand-file-name "savehist" madmacs--cache-dir))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 100))

(use-package recentf
  :ensure nil
  :straight nil
  :custom
  (recentf-save-file (expand-file-name "recentf" madmacs--data-dir))
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))


(provide 'madmacs-files-essentials)
