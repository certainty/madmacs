;; -*- lexical-binding: t; -*-

(use-package dashboard
  :hook (emacs-startup . (lambda ()
                           (dashboard-refresh-buffer)
                           (madmacs--dashboard-hide-modeline)))
  :custom
  (dashboard-banner-logo-title
    "This is my emacs config. There are many like it, but this one is mine.
  Keybindings to try: [C-c j] Global Leader [M-g] Goto [M-s] Search [C-j I] Gptel [C-j i] Copilot")
  
  (dashboard-center-content t)
  (dashboard-items '((projects . 10)
                     (bookmarks . 8)
                     (recents  . 8)))
  (dashboard-icon-type 'nerd-icons)
  (dashboard-startup-banner (concat user-emacs-directory "assets/logo.png"))
  (dashboard-projects-backend 'project-el)

  :config
  (defun madmacs--dashboard-hide-modeline ()
    (interactive)
    (let ((dash-buffer (get-buffer "*dashboard*")))
      (when dash-buffer
        (with-current-buffer dash-buffer
          (setq-local hl-line-mode -1)
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)))))

  (defun madmacs-goto-dashboard ()
    (interactive)
    (dashboard-refresh-buffer)
    (switch-to-buffer "*dashboard*")
    (delete-other-windows)))

(provide 'madmacs-base-dashboard)
