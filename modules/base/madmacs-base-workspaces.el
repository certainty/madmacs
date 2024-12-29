;; -*- lexical-binding: t; -*-

(use-package project
  :straight (:type built-in)
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

(use-package dotenv
  :straight (dotenv :type git :host github :repo "pkulev/dotenv.el")
  :after project
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
  :straight (:type built-in)
  :commands (desktop-save-mode)
  :custom
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

(use-package tab-bar
  :straight (:type built-in)
  :commands
  (tab-bar-new-tab
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
  :hook emacs-startup
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-initialize-project-with-todo t)
  :bind
  (:map goto-map
    ("p" . tabspaces-open-or-create-project-and-workspace)))

(provide 'madmacs-base-workspaces)
