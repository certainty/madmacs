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
    ("t" . madmacs-capture-task))

  (:map goto-map
    ("a" . madmacs-personal-agenda)
    ("t" . madmacs-goto-tasks)
    (""))

  (:map search-map
    ("n G" . madmacs-org-grep))

  (:map madmacs-keymap-gtd
    ("a" . org-agenda)
    ("c" . org-capture)
    ("t" . madmacs-capture-task)
    ("p" . madmacs-capture-project))
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
  (org-tag-alist '(("@desk" . ?d)
                    ("@home" . ?h)
                    ("@office" . ?o)
                    ("@market" . ?m)
                    ("@petstore" . ?p)
                    ("finance" . ?f)
                    ("health" . ?H)
                    ("mind" . ?M)
                    ("body" . ?b)
                    ("family" . ?a)
                    ("career" . ?c)))
  
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
  ;; More specialised capture templates will be added in the appropriate modules
  (org-capture-templates
    '(("t" "Task" entry (file+headline "tasks.org" "Main")
        "* TODO %?
:PROPERTIES:
:CAPTURED: %U
:END:"
        :prepend t)

       ("o" "Codebase Task" entry (file+function "tasks.org" madmacs-ensure-project-heading)
         "* TODO %?
:PROPERTIES:
:CAPTURED: %U
:PROJECT_ROOT: %(project-root (project-current t))
:FILE: [[%F][%f]]
:POS: %l
:END:

%i
"
         :jump-to-captured nil)
       
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
        ((tags-todo "+SCHEDULED<=\"<today>\"|+DEADLINE<=\"<today>\""
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

       ("m" "Meetings"
         ((tags "+CATEGORY=\"meeting\"|+TODO=\"MEETING\""
            ((org-agenda-overriding-header "Meetings \n")
              (org-agenda-use-time-grid t)
              (org-agenda-remove-tags nil)
              (org-agenda-prefix-format "%-15c %s")
              (org-agenda-sorting-strategy '(scheduled-down))))))

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

  (defun madmacs-capture-project ()
    (interactive)
    (org-capture nil "p"))

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

  (defun madmacs-get-project-info ()
    "Get project information including parent directory structure."
    (interactive)
    (let* ((project-root (project-root (project-current t)))
            (project-dir (file-name-nondirectory (directory-file-name project-root)))
            (parent-dir (file-name-nondirectory 
                          (directory-file-name 
                            (file-name-directory 
                              (directory-file-name project-root)))))
            (project-name (if (string= parent-dir "")
                            project-dir
                            (format "%s > %s" parent-dir project-dir))))
      project-name))

   (defun madmacs-ensure-project-heading ()
     (let* ((project-name (madmacs-get-project-info))
             (olp `("Codebases" ,project-name))
             (marker (ignore-errors (org-find-olp olp t))))
       (if marker
         (goto-char marker)
         (org-ql-select (current-buffer)
           '(and (level 1) (heading "Codebases"))
           :action `(progn
                      (org-insert-subheading "")
                      (insert ,project-name)
                      (insert "\n"))))
       (goto-char (org-find-olp olp t))))
  
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
                (window-width . 0.4)
                (reusable-frames . visible))))

(provide 'madmacs-org-gtd)
