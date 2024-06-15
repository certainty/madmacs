(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "This is my emacs config. There are many like it, but this one is mine.")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 8)
                     (projects . 8)
                     (bookmarks . 5)))

  (dashboard-icon-type 'all-the-icons)
  (dashboard-startup-banner (concat user-emacs-directory "assets/logo.png"))
  :config
  (dashboard-setup-startup-hook))

(provide 'madmacs-ux-dashboard)
