;; -*- lexical-binding: t; -*-

(defvar-keymap madmacs-org-mode-keys :doc "Keys for org mode interactions")

(use-package org
  :ensure nil
  :straight nil
  :bind
  (:map goto-map
    ("o" . consult-org-heading)
    ("O" . consult-org-agenda)
    ("A" . org-agenda))
  :config

  ;; general settings
  (setq org-directory "~/org")

  ;; tags
  (setq org-tag-alist '(("@proj" . ?p) ("@area" . ?a) ("@ref" . ?r) ("@gtd" . ?g) ("@cal" . ?c)))

  ;; states
  (setq org-todo-keywords
    '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
      (sequence "PROJ(p)" "IN PROGRESS(i)" "|" "DONE(d)")
      (sequence "|" "CANCELED(c)")
      (sequence "|" "KILL(c)")))

  ;; Agenda
  (setq madmacs-org-areas "~/org/areas.org")
  (setq madmacs-org-projects "~/org/projects.org")
  (setq madmacs-org-references "~/org/references.org")
  (setq madmacs-org-calendar "~/org/calendar.org")
  (setq madmacs-org-inbox "~/org/inbox.org")
  
  (setq org-agenda-files
    (list madmacs-org-areas madmacs-org-projects madmacs-org-references madmacs-org-calendar madmacs-org-inbox))

  ;; embark
  (keymap-set embark-org-heading-map
    "i" #'org-id-get-create)
  
  ;; capture templates
  (setq org-capture-templates
    `(("e" "Jot" entry
        (file+olp+datetree  ,madmacs-org-calendar)
        "* %?"
        :tree-type week)
       
       ("t" "Task" entry
         (file+olp+datetree  ,madmacs-org-calendar)
         "* TODO %?
:properties:
:created: %T
:end:
"
         :tree-type week
         :prepend t
         :prepare-finalize (lambda () (org-id-get-create))
         )
       
       ("p" "Project" entry
         (file ,madmacs-org-projects)
         "* PROJ %?
:properties:
:created: %t
:end:
"
         :prepare-finalize (lambda () (org-id-get-create)))
       
       ("a" "Area" entry
         (file ,madmacs-org-areas)
         "* %?"
         :prepare-finalize (lambda () (org-id-get-create)))
       
       ("r" "Ref" entry
         (file ,madmacs-org-references)
         "* %?
:properties:
:created: %t
:end:
"
         :prepare-finalize (lambda () (org-id-get-create)))
       )
    )
  
  (madmacs-bind-local-leader-map org-mode-map madmacs-org-mode-keys))

(use-package org-ql
  :ensure t)

(use-package org-sidebar
  :ensure t
  :after org-ql
  :config
  (defun madmacs-parse-org-timestamp (timestamp)
    (let* ((parsed-time (parse-time-string timestamp))
            (d (decoded-time-day parsed-time))
            (m (decoded-time-month parsed-time))
            (y (decoded-time-year parsed-time)))
      (encode-time 0 0 0 d m y)))
       
  (defun madmacs-org-current-node-info (node)
    "Retrieve information about the current Org node."
    (interactive)
    (let* ((element (or node (org-element-at-point)))
            (headline (org-element-property :raw-value element))
            (tags (org-get-tags element))
            (properties (org-entry-properties element 'standard))
            (created (assoc "CREATED" properties))
            (id (assoc "ID" properties)))
      (list :headline headline :tags tags :created (and created (madmacs-parse-org-timestamp (cdr created))) :id (cdr id))))
  
  (defun madmacs-org-related-nodes-query (node)
    (interactive)
    (let* ((props (madmacs-org-current-node-info node))
            (id (plist-get props :id))
            (created (plist-get props :created)))
      `(or
         ,@(when id
             `((link :target ,(concat "id:" id)))) ; back links

         ,@(when created
             `((and
                 (tags "@cal")
                 (or
                   (heading ,(format-time-string "%Y-W%g" created))
                   (heading ,(format-time-string "%Y-%m-%d %A" created))))))
         
         (tags-local ,@(plist-get props :tags)))))
         
  (defun madmacs-org-related-nodes ()
    (interactive)
    (org-sidebar-ql (org-agenda-files) (madmacs-org-related-nodes-query (org-element-at-point)) :title "Related nodes")))

(use-package org-tidy
  :ensure t
  :hook (org-mode . org-tidy-mode)
  :config
  (which-key-add-keymap-based-replacements madmacs-org-mode-keys
    "c" '("Tidy toggle" . org-tidy-toggle)))


(provide 'madmacs-org-personal)
