;; -*- lexical-binding: t; -*-

(use-package project
  :ensure nil
  :straight nil
  :custom
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))

  (project-vc-extra-root-markers '(".dir-locals.el" ".project" "package.json" "autogen.sh")))

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

(provide 'madmacs-projects-essentials)
