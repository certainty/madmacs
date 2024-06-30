
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

  :config
  (tabspaces-mode))


(provide 'madmacs-ux-workspaces)
