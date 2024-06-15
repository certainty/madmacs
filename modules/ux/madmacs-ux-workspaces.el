
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

(provide 'madmacs-ux-workspaces)
