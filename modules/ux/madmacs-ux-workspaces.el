;; -*- lexical-binding: t; -*-

(use-package project
  :ensure nil
  :straight nil
  :bind
  (:map goto-map
    ("f" . project-find-file))
  (:map search-map
    ("R" . project-query-replace-regexp))
  
  :custom
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))

  (project-vc-extra-root-markers '(".project" "package.json" "autogen.sh" ".projectile" "go.mod")))

;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode))

(use-package dotenv
  :ensure t
  :straight (dotenv :type git :host github :repo "pkulev/dotenv.el")
  :after project
  ;; TODO: verify that this works
  :hook (project-switch-project . madmacs--dotenv-project-hook)
  :config

  (defun madmacs--dotenv-project-hook ()
    "Update project environment variables."
    (interactive)
    (message "Updating project environment variables...")
    (when (project-root)
      (dotenv-update-project-env
       (project-root)))))

(use-package desktop
  :ensure nil
  :straight nil
  :commands (desktop-save-mode)
  :custom
  (desktop-dirname (madmacs--ensure-cache-dir "desktops"))
  (desktop-base-file-name "emacs.desktop")
  (desktop-base-lock-name "lock")
  (desktop-path (list desktop-dirname))
  (desktop-save 'ask-if-new)
  (desktop-files-not-to-save  (concat "^$" ".*magit$"))
  (desktop-restore-eager 4)
  (desktop-load-locked-desktop t)
  (desktop-buffers-not-to-save
    (concat "\\("
      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
      "\\)$")))

(use-package multisession
  :ensure nil
  :straight nil
  :custom
  (multisession-directory (madmacs--ensure-cache-dir "multisession")))

(use-package tab-bar
  :ensure nil
  :straight nil
  :after (project)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t) ;; show numbers in tabs
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    lem--tab-bar-suffix
                    tab-bar-format-add-tab)))

(use-package tabspaces
  :custom
  (tabspaces-use-filtered-buffers-as-default t)

  :bind
  (:map goto-map
    ("p" . tabspaces-open-or-create-project-and-workspace))

  :config
  (tabspaces-mode))


(provide 'madmacs-ux-workspaces)
