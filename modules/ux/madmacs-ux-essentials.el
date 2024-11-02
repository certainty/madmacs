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

(use-package so-long
  :ensure nil
  :straight nil
  :hook (after-init . global-so-long-mode))

(use-package simple
  :ensure nil
  :straight nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (line-move-visual t))

(use-package display-line-numbers
  :ensure nil
  :straight nil
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t))

(use-package casual-isearch
  :ensure t
  :bind (:map isearch-mode-map ("M-o" . casual-isearch-tmenu)))

(use-package casual-info
  :ensure t
  :bind (:map Info-mode-map ("M-o" . casual-info-tmenu)))

(use-package casual-bookmarks
  :ensure t
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map ("M-o" . casual-bookmarks-tmenu)))

(use-package emacs
  :ensure nil
  :custom
  (sentence-end-double-space nil)
  (tab-width 2)
  (fill-column 150)
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (require-final-newline nil)
  (visible-bell nil)
  (use-short-answers t)
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  :config
  (blink-cursor-mode 0)
  (global-hl-line-mode -1)
  (setq-default indent-tabs-mode nil))

(provide 'madmacs-ux-essentials)
