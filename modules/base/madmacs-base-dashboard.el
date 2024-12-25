;; -*- lexical-binding: t; -*-

(use-package dashboard
  :custom
  (dashboard-banner-logo-title
    "       This is my emacs config. There are many like it, but this one is mine.
  Keybindings to try: [C-j] Global Leader [M-g] Goto [M-s] Search [C-j I] Gptel [C-j i] Copilot")
  
  (dashboard-center-content t)
  (dashboard-items '((projects . 10)
                     (bookmarks . 8)
                     (recents  . 8)))
  (dashboard-icon-type 'nerd-icons)
  (dashboard-startup-banner
    (expand-file-name "assets/logo.txt" user-emacs-directory))
  
  (dashboard-projects-backend 'project-el)

  :init
  (dashboard-setup-startup-hook))


(provide 'madmacs-base-dashboard)
