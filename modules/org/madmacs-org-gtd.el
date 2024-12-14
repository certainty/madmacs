;; -*- lexical-binding: t; -*-

;;; This is my GTD setup
;;;
;;; With the following base properties:
;;; 1. Tasks and notes are separated
 ;;; 1.1 I use a tasks.org file for projects and tasks
;;; 2. Meeting minutes are taken in separate file called meetings.org
;;; 2.2 reverse date tree to have most recent meetings on top
;;; 3. Notes are separated into fleeting and permanent
;;; 3.1 Fleeting notes are added to journal.org
;;; 3.2 Permanent notes are done via denote and stored in the /notes directory
;;;     This directory contains all the reference notes that I might need


(use-package org
  :ensure nil
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

  (defvar-keymap madmacs-keymap-gtd :doc "Get things done")
  (which-key-add-keymap-based-replacements madmacs-keymap-global
    "g" `("GTD" . ,madmacs-keymap-gtd))

  :custom
  (org-id-link-to-org-use-id t)

  ;; TODOs
  (org-todo-keywords
    '((sequence "TODO(t)" "DOING(g!)" "|" "DONE(d!)")
       (sequence "PROJ(p)" "DOING(g!)" "|" "DONE(d!)")
       (sequence "EPIC(e)" "DISCOVERY(y!)" "|" "DONE(d!)")
       (sequence "MEETING(m)" "|" "DONE(d)")
       (sequence "APPT(a)" "|" "DONE(d)")
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

       ("T" "Task (today)" entry (file+headline "tasks.org" "Main")
         "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:CAPTURED: %U
:END:"
         :prepend t)

       ("p" "Project" entry (file+headline "tasks.org" "Projects")
       "* PROJ %?
:PROPERTIES:
:CAPTURED: %U
:CATEGORY: project
:PROJECT_KEY: %^{Project Key}
:END:"  )
       
       ("e" "Epic" entry (file+headline "tasks.org" "Epics")
         "* EPIC %?
:PROPERTIES:
:CAPTURED: %U
:CATEGORY: epic
:EPIC_KEY: %^{Epic Key}
:END:")
       
       ("m" "Meeting")
       
       ("mm" "Adhoc" entry (file+olp+datetree "meetings.org" "Adhoc")
         "* %T %?
:PROPERTIES:
:CATEGORY: meetings
:RECURRING: no
:CAPTURED: %U
:END:
"
         :prepend t
         :tree-type week)
       
       ("mM" "Adhoc (date)" entry (file+olp+datetree "meetings.org" "Adhoc")
         "* %T %?
:PROPERTIES:
:CATEGORY: meetings
:RECURRING: no
:CAPTURED: %U
:END:
"
         :prepend t
         :tree-type week
         :time-prompt t)

       ("mo" "1on1" entry (file+olp+datetree "meetings.org" "1on1")
         "* %T :1on1:
:PROPERTIES:
:CATEGORY: meetings
:CAPTURED: %U
:RECURRING: yes
:END:

%?
"
         :prepend t
         :tree-type month)

       ("mO" "1on1" entry (file+olp+datetree "meetings.org" "1on1")
         "* %T :1on1:
:PROPERTIES:f
:CATEGORY: meetings
:CAPTURED: %U
:RECURRING: yes
:END:

%?"
         :prepend t
         :time-prompt t
         :tree-type month)
       
       ("n" "Notes")
       ("nj" "Fleeting note" entry (file+olp+datetree "journal.org")
         "* %U %?"
         :tree-type week)
       
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

;; Permanent notes
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map madmacs-keymap-notes
    ("n" . denote)
    ("c" . denote-region) ; "contents" mnemonic
    ("N" . denote-type)
    ("d" . denote-date)
    ("z" . denote-signature) ; "zettelkasten" mnemonic
    ("s" . denote-subdirectory)
    ("i" . denote-link) ; "insert" mnemonic
    ("I" . denote-add-links)
    ("b" . denote-backlinks)
    ("f f" . denote-find-link)
    ("f b" . denote-find-backlink)
    ("r" . denote-rename-file)
    ("R" . denote-rename-file-using-front-matter))

  (:map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory (concat org-directory "/notes"))
  (denote-save-buffers nil)
  (denote-known-keywords '("nw1" "messaging" "xws" "pmd" "cloud_migration" "emacs" "health" "finance" "home" "family" "dea"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  (denote-dired-directories (list denote-directory (concat org-directory "/.attachments")))

  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%i\n%?")
    (add-to-list 'org-capture-templates
      '("n" "Permanent Note" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save nil
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured nil)))

  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-notes
  :ensure t
  :after (org)
  :bind
  (:map goto-map
    ("n n" . consult-notes))
  (:map search-map
    ("n g" . consult-notes-search-in-all-notes))

  :config
  (setopt consult-notes-file-dir-sources
    `(("Org"       ?o "~/org")
      ("Notes"    ?n "~/org/notes")))
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode))

;; Aux
(use-package org-ql
  :ensure t
  :after org)

(use-package org-rich-yank
  :ensure t
  :after org
  :bind (:map org-mode-map
          ("C-M-y" . org-rich-yank)))

(use-package yankpad
  :bind
  (("C-c y" . yankpad-insert))

  :custom
  (yankpad-file (concat org-directory "/snippets.org")))

(provide 'madmacs-org-gtd)
