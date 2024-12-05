;; -*- lexical-binding: t; -*-

(defvar-keymap madmacs-org-mode-keys :doc "Keys for org mode interactions")

;; TODO: steel ideas from: https://github.com/psamim/dotfiles/blob/master/doom/config.el#L441

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
  (org-tag-alist '(("work" . ?w) ("personal" . ?p) ("gtd" . ?g)))
 
  
  (org-todo-keywords
    '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
       (sequence "PROJ(p)" "IN PROGRESS(i)" "|" "DONE(d)")
       (sequence "MEETING(m)" "|" "DONE(d)")
       (sequence "|" "CANCELED(c)")
       (sequence "|" "KILL(k)")))
  (org-priority-default ?B)
  (org-priority-highest ?A)
  (org-priority-lowest ?C)

  (org-agenda-sorting-strategy
    '((agenda habit-down time-up ts-up priority-down category-keep)
       (todo priority-down category-keep)
       (tags priority-down category-keep)
       (search category-keep)))

  (org-agenda-custom-commands
    '(("t" "My Agenda"
        ((tags "+TODO=\"TODO\""
            ((org-agenda-overriding-header "⚡ Today\n")
              (org-agenda-sorting-strategy '(priority-down))
              (org-agenda-remove-tags t)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'scheduled))
              (org-agenda-prefix-format "  %?-2i %?-12c %b")))

          (tags "+TODO=\"PROJ\""
            ((org-agenda-overriding-header "\n⚡ Projects\n")
              (org-agenda-remove-tags t)
              (org-tags-match-list-sublevels nil)
              (org-agenda-show-inherited-tags nil)
              (org-agenda-prefix-format "  %?-2i %s ")
              (org-agenda-todo-keyword-format "")))
          (agenda ""
           ((org-agenda-overriding-header "\n🗓️ Calendar\n")
             (org-agenda-skip-scheduled-if-done nil)
             (org-agenda-time-leading-zero t)
             (org-agenda-timegrid-use-ampm nil)
             (org-agenda-skip-timestamp-if-done t)
             (org-agenda-skip-deadline-if-done t)
             (org-agenda-start-day "+0d")
             (org-agenda-span 5)
             (org-agenda-repeating-timestamp-show-all nil)
             (org-agenda-remove-tags t)
             (org-agenda-prefix-format "   %?-2i %?-12c %?-2t%s %b")
             (org-agenda-todo-keyword-format "")
             (org-agenda-time)
             (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
             (org-agenda-scheduled-leaders '("" ""))
             (org-agenda-deadline-leaders '(" Deadline: " "  In %2d d.: " " %2d d. ago: "))
             (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))

         ("u" "Unscheduled Tasks"
           ((tags-todo "-SCHEDULED={.+}"
              ((org-agenda-overriding-header "⚡Unscheduled Tasks\n")
                (org-agenda-sort-strategy '(priority-down))
                (org-agenda-group-by-heading t)
                (org-tags-match-list-sublevels t)
                (org-agenda-prefix-format "%i %-30c "))))
           nil
           ((org-tags-match-list-sublevels t)
             (org-agenda-show-tags t)))))
  
  
  :config
  ;; embark
  (with-eval-after-load 'embark
    (keymap-set embark-org-heading-map
      "i" #'org-id-get-create))
  
  (add-to-list 'popper-reference-buffers 'org-agenda-mode)
  (add-to-list 'popper-reference-buffers "\\*Org Agenda\\*")

  (add-to-list 'display-buffer-alist
             '("\\*Org Agenda\\*"
                (display-buffer-reuse-window
                  display-buffer-in-side-window)
               (side . right)
               (slot . 0)
                (window-width . 0.33)
                (reusable-frames . visible)))
  
  ;; capture templates
  (madmacs-bind-local-leader-map org-mode-map madmacs-org-mode-keys))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/life"))
  (org-roam-dailies-directory "calendar/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-update-on-save t)
  (org-roam-completion-everywhere nil)
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
         :unnarrowed t)
       ("c" "contact" entry "* @${title}
%U
:PROPERTIES:
:ID: %(org-id-uuid)
:category: contact
:END:

%?"
         :target (file+head "references/contacts.org" "#+title: contacts")
         :unnarrowed t)
       ))

  (org-roam-dailies-capture-templates
    `(("d" "default" entry "* %?" :target (file+head "%<%Y-%m-%d>.org" "%U\n#+title: %<%Y-%m-%d>"))
       ("t" "task" entry "* TODO %?" :target (file+head "%<%Y-%m-%d>.org" "%U\n#+title: %<%Y-%m-%d>"))
       ("m" "meeting" entry (file ,(concat org-roam-directory "/templates/meeting.org"))
         :target (file+head "%<%Y-%m-%d>.org" "%U\n#+title: %<%Y-%m-%d>"))))

  (org-roam-node-display-template
    (concat "${directories:10} ::  ${hierarchy:*} " (propertize "${tags:10}" 'face 'org-tag)))

  :bind
  (:map org-mode-map
    ("C-M-i" . org-roam-complete-link-at-point))

  :init
  (org-roam-db-autosync-mode)
  
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "%s " (car (split-string dirs "/")))
      ""))
  
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
        (when (> level 0) (concat (org-roam-node-file-title node) " > "))
        (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
        (org-roam-node-title node))))

  :config
 
  (defun madmacs-org-roam-dailies-goto-date (&optional prefer-future)
    (interactive)
    (org-roam-dailies-goto-date prefer-future "d"))

  (defun madmacs-org-roam-dailies-goto-today ()
    (interactive)
    (org-roam-dailies-goto-today "d"))

    (defun madmacs-org-roam-dailies-goto-yesterday (n)
    (interactive)
    (org-roam-dailies-goto-yesterday (or n 1) "d"))
  
  (setopt org-agenda-files
    (mapcar (lambda (n) (concat org-roam-directory n))
      '("/projects" "/calendar" "/areas" "/inbox.org")))
  
  (setopt org-agenda-regexp-filter ".+\.org$")
  
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t)))))

  )

(use-package org-ql
  :ensure t
  :after org)

(use-package org-rich-yank
  :ensure t
  :after org
  :bind (:map org-mode-map
          ("C-M-y" . org-rich-yank)))

(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚑" "⬆" "⬇")))

(provide 'madmacs-org-personal)
