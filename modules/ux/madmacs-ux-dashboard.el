;; -*- lexical-binding: t; -*-

(use-package dashboard
  :ensure t
  :hook (emacs-startup . (lambda ()
                           (dashboard-refresh-buffer)
                           (madmacs--dashboard-hide-modeline)))
  :custom
  (dashboard-banner-logo-title "This is my emacs config. There are many like it, but this one is mine.")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 8)
                     (projects . 8)
                     (bookmarks . 5)))

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


(provide 'madmacs-ux-dashboard)
