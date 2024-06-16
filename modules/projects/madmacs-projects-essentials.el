(use-package project
  :ensure nil
  :straight nil
  :bind
  (:map madmacs-project-map
        ("p" . project-switch-project)
        ("r" . project-query-replace-regexp)
        ("v" . project-vc-dir)
        ("R" . project-remember-projects-under)
        ("z" . project-kill-buffers))

    :custom
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))

    (project-vc-extra-root-markers '(".dir-locals.el" ".project" "package.json" "autogen.sh")))

(provide 'madmacs-projects-essentials)
