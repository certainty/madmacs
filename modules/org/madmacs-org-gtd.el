;; -*- lexical-binding: t; -*-
;;; This is my GTD setup tailored to manage code related tasks
;;; All other tasks I manage in a different app

(use-package org
  :straight (:type built-in)
  :hook (org-after-todo-state-change . save-buffer)
  :bind
  (:map madmacs-keymap-global
    ("a" . madmacs-personal-agenda)
    ("c" . org-capture)
    ("T" . madmacs-capture-task))

  (:map goto-map
    ("a" . madmacs-personal-agenda)
    ("t" . madmacs-goto-tasks)
    (""))

  (:map search-map
    ("n G" . madmacs-org-grep))

  (:map madmacs-keymap-gtd
    ("a" . org-agenda)
    ("c" . org-capture)
    ("T" . madmacs-capture-task))
  
  :init
  (require 'org-id)
  
  (defvar-keymap madmacs-keymap-gtd :doc "Task & Project management")
  (which-key-add-keymap-based-replacements madmacs-keymap-global
    "g" `("GTD" . ,madmacs-keymap-gtd))

  :custom
  (org-id-link-to-org-use-id t)

  ;; TODOs
  (org-todo-keywords
    '((sequence "TODO(t)" "DOING(g!)" "|" "DONE(d!)")
       (sequence "FEATURE(r)" "DOING(g!)" "|" "DONE(d!)")
       (sequence "TECHDEPT(e)" "|" "DONE(d!)")
       (sequence "FIX(f!)" "REVIEW(r!)" "|" "DONE(d)")
       (sequence "POC(p)" "|" "DONE(d)")
       (sequence "REFACTOR(f)" "|" "DONE(d)")
       (sequence "DOC(o)" "|" "DONE(d)")
       (sequence "MEETING(m)" "|" "DONE(d)")
       (sequence "APPT(a)" "|" "DONE(d)") 
       (sequence "|" "BLOCKED(l!)")
       (sequence "|" "CANCELED(c!)")))

  (org-priority-default ?B)
  (org-priority-highest ?A)
  (org-priority-lowest ?F)
  
  
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)


  ;; blocks
  (org-structure-template-alist
    '(("s" . "src")
       ("e" . "src emacs-lisp")
       ("E" . "src emacs-lisp :results value code :lexical t")
       ("t" . "src emacs-lisp :tangle FILENAME")
       ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
       ("x" . "example")
       ("X" . "export")
       ("q" . "quote")))

  ;; Capture for GTD
  (org-capture-templates
    '(("t" "Task" entry (file+headline "tasks.org" "Main")
        "* TODO %?
:PROPERTIES:
:CAPTURED: %U
:END:"
        :prepend t)
       
       ("c" "Contact" entry (file "contacts.org")
         "* %^{Name}
:PROPERTIES:
:CAPTURED: %U
:CATEGORY: contact
:END:

%?
"    )))

  ;; Agenda
  (org-agenda-files
    (list (concat org-directory "/tasks.org")
      (concat org-directory "/meetings.org")
      (concat org-directory "/contacts.org")))

  (org-agenda-current-time-string
    "◀── now ─────────────────────────────────────────────────")

  (org-agenda-sorting-strategy
    '((agenda habit-down time-up ts-up priority-down category-keep)
       (todo priority-down category-keep)
       (tags priority-down category-keep)
       (search category-keep)))

  (org-columns-default-format-for-agenda "%SCHEDULED %25ITEM %TODO %3PRIORITY %TAGS")

  ;; TODO tweak agenda to be more useful for coderelated tasks
  (org-agenda-custom-commands
    '(("y" "My Agenda"
        ((tags-todo "-CATEGORY=\"project\""
            ((org-agenda-overriding-header "⚡ Today\n")
              (org-agenda-sorting-strategy '(priority-down))
              (org-agenda-remove-tags nil)
              (org-agenda-use-time-grid t)
              (org-agenda-deadline-leaders '(" Deadline: " " In %2d d.: " "%2d d. ago: "))
              (org-agenda-skip-function '(org-agenda-skip-entry-if  'todo 'done))
              (org-agenda-prefix-format "%s  %?-2i %?-12c ")))

          (tags "+CATEGORY=\"project\"|+TODO=\"PROJ\""
            ((org-agenda-overriding-header "\n⚡ Projects\n")
              (org-agenda-remove-tags nil)
              (org-tags-match-list-sublevels nil)
              (org-agenda-show-inherited-tags t)
              (org-agenda-prefix-format "  %?-2i %s ")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-todo-keyword-format "")))

          (agenda ""
            ((org-agenda-overriding-header "\n🗓️ Calendar\n")
              (org-deadline-warning-days 3)
              (org-agenda-skip-scheduled-if-done nil)
              (org-agenda-time-leading-zero t)
              (org-agenda-timegrid-use-ampm nil)
              (org-agenda-skip-timestamp-if-done t)
              (org-agenda-skip-deadline-if-done t)
              (org-agenda-start-day "+0d")
              (org-agenda-span 5)
              (org-agenda-repeating-timestamp-show-all nil)
              (org-agenda-remove-tags nil)
              (org-agenda-time t)
              (org-agenda-prefix-format "   %?-2i %?-12c %?-2t%s ")
              (org-agenda-scheduled-leaders '("" ""))
              (org-agenda-deadline-leaders '(" Deadline: " " In %2d d.: " "%2d d. ago: "))
              (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))

       ("w" "Weekly Review"
         ((todo "*"
            ((org-agenda-overriding-header "🎉 Completed Tasks\n")
              (org-agenda-prefix-format "   %?-2i %?-12c %?-2t%s ")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
              (org-agenda-span 'week)))

          (agenda ""
            ((org-agenda-overriding-header "\n📋 Unfinished Scheduled Tasks\n")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-prefix-format "   %?-2i %?-12c %?-2t%s ")
              (org-agenda-span 'week)))))

       ("u" "Unscheduled Tasks"
         ((tags-todo "-SCHEDULED={.+}"
            ((org-agenda-overriding-header "⚡Unscheduled Tasks\n")
              (org-agenda-sort-strategy '(priority-down))
              (org-agenda-group-by-heading t)
              (org-tags-match-list-sublevels t)
              (org-agenda-prefix-format "%i %-15c ")))))))
  :config
  (defun madmacs-personal-agenda ()
    (interactive)
    (org-agenda nil "y"))

  (defun madmacs-capture-task ()
    (interactive)
    (org-capture nil "t"))

  (defun madmacs-goto-tasks ()
    (interactive)
    (find-file-other-window (concat org-directory "/tasks.org")))

  (defun madmacs-org-ripgrep ()
    (interactive)
    (consult-ripgrep org-directory))

  (defun madmacs-org-grep ()
    (interactive)
    (let ((default-directory org-directory))
      (call-interactively #'grep)))

  ;; embark
  (with-eval-after-load 'embark
    (keymap-set embark-org-heading-map
      "i" #'org-id-get-create))

  (with-eval-after-load 'popper
    (cl-pushnew 'org-agenda-mode popper-reference-buffers)
    (cl-pushnew "\\*Org Agenda\\*" popper-reference-buffers))

  (add-to-list 'display-buffer-alist
             '("\\*Org Agenda\\*"
                (display-buffer-reuse-window
                  display-buffer-in-side-window)
               (side . right)
               (slot . 0)
                (window-width . 0.4)
                (reusable-frames . visible))))

(use-package org-project-capture
  :bind
  (:map madmacs-keymap-gtd
    ("p" . org-project-capture-capture-for-current-project)
    ("P" . org-project-capture-project-todo-completing-read))
  
  (:map madmacs-keymap-global
    ("t" .  org-project-capture-capture-for-current-project))
  
  :custom
  (org-project-capture-backend (make-instance 'org-project-capture-project-backend))
  (org-project-capture-capture-template "* TODO %?
:PROPERTIES:
:CAPTURED: %U
:FILE: [[%F][%f]]
:POS: %l
:END:

%i
")
  :config
  (org-project-capture-per-project)

  ;; upstream version is outdated so we use this
  (cl-defmethod org-project-capture-current-project ((_backend org-project-capture-project-backend))
    (project-name (project-current))))

(provide 'madmacs-org-gtd)
