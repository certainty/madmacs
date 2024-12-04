;; -*- lexical-binding: t; -*-

(defvar-keymap madmacs-org-mode-keys :doc "Keys for org mode interactions")

;; we use this as a very barebones org setup and all the rest is configured in the org-roam section
(use-package org
  :ensure nil
  :bind
  (:map goto-map
    ("o" . consult-org-heading)
    ("O" . consult-org-agenda)
    ("a" . org-agenda))

  :custom
  (org-directory "~/org")
  (org-tag-alist '(("@proj" . ?p) ("@area" . ?a) ("@ref" . ?r) ("@gtd" . ?g) ("@cal" . ?c) )
  (org-todo-keywords
    '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
       (sequence "PROJ(p)" "IN PROGRESS(i)" "|" "DONE(d)")
       (sequence "MEETING(m)" "|" "DONE(d)")
       (sequence "|" "CANCELED(c)")
       (sequence "|" "KILL(k)")))
  
  :config
  ;; embark
  (with-eval-after-load 'embark
    (keymap-set embark-org-heading-map
      "i" #'org-id-get-create))
  
  ;; capture templates
  (madmacs-bind-local-leader-map org-mode-map madmacs-org-mode-keys))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/life"))
  (org-roam-dailies-directory "calendar/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-update-on-save t)
  (org-roam-completion-everywhere t)
  (org-roam-mode-sections
    '((org-roam-backlinks-section :unique t)
       org-roam-reflinks-section))

  (org-roam-capture-templates
    `(("a" "area" plain (file ,(concat org-roam-directory "/templates/area.org"))
         :target (file+head "areas/${slug}.org" "")
        :unnarrowed t)
       ("p" "project" plain (file ,(concat org-roam-directory "/templates/project.org"))
         :target (file+head "projects/${slug}.org" "")
         :unnarrowed t)
       ("r" "reference" plain (file ,(concat org-roam-directory "/templates/reference.org"))
         :target (file+head "references/%<%Y%m%d%H%M%S>-${slug}.org" "")
         :unnarrowed t)))

  (org-roam-dailies-capture-templates
    `(("d" "default" entry "* %?" :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
       ("t" "task" entry "* TODO %?" :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
       ("m" "meeting" entry (file ,(concat org-roam-directory "/templates/meeting.org"))
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
       ))

  :bind
  (:map org-mode-map
    ("C-M-i" . org-roam-complete-link-at-point))

  :init
  (org-roam-db-autosync-mode)

  :config
  (setopt org-agenda-files
    (mapcar (lambda (n) (concat org-roam-directory n))
      '("/projects" "/calendar" "/areas" "inbox.org")))
  
  (setopt org-agenda-regexp-filter ".+\.org$")
  
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t))))))

(use-package org-ql
  :ensure t
  :after org)

(use-package org-rich-yank
  :ensure t
  :after org
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(provide 'madmacs-org-personal)
