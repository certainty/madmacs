;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing and note taking
;;;
;;; I manage documents and notes in essentially two different ways.
;;; Documents follow the PARA structure, because I don't tend to alter them. I just categorize them and make them available at the place
;;; that makes the most sense at any given point in time.
;;;
;;; Notes on the other hand do change, in fact I create them and I employ a more lose structure to them.
;;; This is why I do not use the PARA structure for notes. Instead I use a more associate structure, where I can link notes to each other.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar madmacs-notes-path (concat madmacs-shared-silo-path "/Notes") "Path to notes and documents")
(defvar madmacs-library-path (concat madmacs-shared-silo-path "/Library") "Path to the library of books and papers")
(defvar madmacs-storage-path (concat madmacs-shared-silo-path "/Storage"))
(defvar madmacs-storage-current-path (concat madmacs-storage-path "/Current") "Path to current documents and information that is relevant for the current time and context")
(defvar madmacs-storage-archive-path (concat madmacs-storage-path "/Archives") "Path to archives which contain all documents and information that is not current. This is not trash.")
(defvar madmacs-storage-resources-path (concat madmacs-storage-path "/Resources") "Path to resources, that are mostly independent in scope for time and context")

(use-package emacs
  :straight nil
  :demand t
  :custom
  (dictionary-server "dict.org")

  :init
  (defvar-keymap madmacs-keymap-notes :doc "Writing and note taking")
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements madmacs-keymap-global
      "n" `("Notes+Writing" . ,madmacs-keymap-notes))))

(use-package flyspell
  :straight nil
  :hook
  (text-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode)
  :config
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-command "pandoc -f markdown -t html"))

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package google-translate
  :after embark
  :bind
  (:map embark-region-map
    ("t" . google-translate-at-point))
  
  :config
  (setq google-translate-default-target-language "en")
  (setq google-translate-backend-method 'curl)
  (setq google-translate-listen-program "mpv")
  (setq google-translate-output-destination 'echo-area)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-enable-ido-completion t)
  (setq google-translate-show-phonetic t)
  (setq google-translate-phonetic-fontify-function 'google-translate-fontify-buffer)
  (setq google-translate-translation-directions-alist
    '(("de" . "en") ("en" . "de")
       ("de" . "fr") ("fr" . "de")
       ("en" . "is") ("is" . "en"))))

(use-package writegood-mode
  :hook
  (text-mode . writegood-mode))

(use-package org
  :straight (:type built-in)
  :config
  (with-eval-after-load 'org-capture
    (setopt org-capture-templates
      (append
        org-capture-templates
        '(
           ("n" "Notes")
           ("nj" "Fleeting note" entry (file+olp+datetree "journal.org")
             "* %U %?"
             :tree-type week))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permanent notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map madmacs-keymap-global
    ("C" . denote)
    ("m" . madmacs-notes-meeting-new-recuraring))
  
  (:map madmacs-keymap-notes
    ("." . denote-add-front-matter) ; "make this denote" mnemonic
    ("n" . denote-open-or-create)
    ("c" . denote)
    ("C" . denote-subdirectory)
    ("d" . denote-date)
    ("r" . denote-region) ; "contents" mnemonic

    ("i" . denote-link) ; "insert" mnemonic
    ("I" . denote-link-after-creating)
    ("t" . denote-type)
    ("z" . denote-signature) ; "zettelkasten" mnemonic
    ("l" . denote-add-links)
    ("b" . denote-backlinks)
    
    ("f f" . denote-find-link) ;; find link to current file
    ("f b" . denote-find-backlink)
    ("f r" . madmacs-find-recent-denote-files)
    
    ("m" . denote-rename-file)
    ("M" . denote-rename-file-using-front-matter))

  (:map goto-map
    ("nr" . madmacs-find-recent-denote-files))

  (:map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  
  :custom
  (denote-directory madmacs-notes-path)
  (denote-save-buffers nil)
  (denote-known-keywords '("area" "project" "archive" "resource" "nw1" "messaging" "xws" "pmd" "cloud_migration" "emacs" "health" "finance" "home" "family" "dea"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(template title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  (denote-dired-directories
    (list denote-directory
      madmacs-storage-path
      madmacs-storage-current-path
      madmacs-storage-archive-path
      madmacs-storage-resources-path
      madmacs-library-path))
  
  (denote-templates
    '((proj . "* Objective\n\n* References\n")
      (meeting . "* Outcome\n\n* Notes\n\n* Tasks\n** Our\n** Theirs\n\n" )
      (plain . "")))
  
  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%i\n%?")
    
    (cl-pushnew
      '("nn" "Permanent" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save nil
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured nil)
       org-capture-templates)
    
    (cl-pushnew
      '("nm" "Meeting Note" plain
         (file denote-last-path)
         (function
           (lambda ()
             (let ((denote-use-directory (expand-file-name "Meetings" (denote-directory)))
                    (denote-use-keywords (list "meeting"))
                    (denote-use-template 'meeting))
               (denote-org-capture))))
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)
      org-capture-templates))

  :config
  (require 'denote-silo-extras)
  (denote-rename-buffer-mode 1)

  (defvar madmacs-notes-meeting-recurring '("1on1" "monday sync")
    "The list of recurring meetings that I take notes on")

  (defvar madmacs-notes-meeting-prompt-history nil)

  (defun madmacs-notes-meeting-prompt-recurring ()
    "Prompt for the recurring meeting to add a note to"
     (let ((default-value (car madmacs-notes-meeting-recurring)))
       (completing-read
         (format-prompt "New entry for recurring meeting" default-value)
         madmacs-notes-meeting-recurring
         nil :require-match nil
         'madmacs-notes-meeting-prompt-history
         default-value)))

  (defun madmacs-notes-meeting-recurring-get-file (name)
  "Find file in variable `madmacs-notes-meeting-directory' for NAME recurring meeting.
If there are more than one files, prompt with completion for one among
them.

NAME is one among `madmacs-notes-meeting-recurring'."
    (if-let ((files (denote-directory-files (format "%s.*_meeting" name)))
              (length-of-files (length files)))
      (cond
        ((= length-of-files 1)
          (car files))
        ((> length-of-files 1)
          (completing-read "Select a file: " files nil :require-match)))
      (user-error "No files for recurring meeting with name `%s'" name)))

  (defun madmacs-notes-meeting-new-recurring ()
  "Prompt for the name of a recurring heading and insert a timestamped heading therein.
The name of meeting corresponds to at least one file in the variable
`denote-directory'.  In case there are multiple files, prompt to choose
one among them and operate therein.

Names are defined in `madmacs-notes-meeting-recurring'."
    (declare (interactive-only t))
    (interactive)
    (let* ((name (madmacs-notes-meeting-prompt-recurring))
            (file (madmacs-notes-meeting-recurring-get-file name))
            (time (format-time-string "%F %a %R")))  ; remove %R if you do not want the time
      (with-current-buffer (find-file file)
        (goto-char (point-max))
        (insert (format "* [%s]\n\n" time)))))

  (defun find-recent-denote-files (&optional days)
  "Show recent files in `denote-directory' modified within DAYS (default 8)."
    (interactive "P")
    (let ((days (or days 8))
           (find-ls-option '("-lt" . "-lt")))
      (find-dired denote-directory (format "-type f -mtime -%d" days))))
  
  (defun madmacs-find-recent-denote-files (&optional days)
    "Show recent files in `denote-directory' modified within DAYS (default 8)."
    (interactive "P")
    (let* ((days (or days 8))
            (cmd (format "find . -type f -mtime -%d -print0 | xargs -0 ls -lt" days))
            (buf (get-buffer-create "*Recent Notes*")))
      (with-current-buffer buf
        (unless (eq major-mode 'dired-mode)
          (dired-mode))
        (let ((default-directory denote-directory)
               (inhibit-read-only t))
          (erase-buffer)
          (call-process-shell-command cmd nil t)))
      (pop-to-buffer buf))))

(use-package consult-notes
  :after org
  :bind
  (:map goto-map
    ("n n" . consult-notes))
  (:map search-map
    ("n g" . consult-notes-search-in-all-notes))

  :custom
  (consult-notes-denote-files-function
    (function
      (lambda ()
        (denote-directory-files nil nil t "#.*#$")))) ;; text-only and no auto-saved files

  (consult-notes-file-dir-sources
    `(("Org"        ?o ,org-directory)
       ("Current"   ?c ,madmacs-storage-current-path)
       ("Resources" ?r ,madmacs-storage-resources-path)
       ("Library"   ?l ,madmacs-library-path)
       ("Archives"  ?c ,madmacs-storage-archive-path :hidden t)))
  
  :config
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode))

(use-package denote-explore
  :bind
  (:map madmacs-keymap-notes
    ("s c" . denote-explore-count-notes)
    ("s C" . denote-explore-count-keywords)
    ("s b" . denote-explore-barchart-keywords)
    ("s e" . denote-explore-barchart-filetypes)
    ;; random
    ("R r" . denote-explore-random-note)
    ("R l" . denote-explore-random-link)
    ("R k" . denote-explore-random-keyword)
    ("R x" . denote-explore-random-regex)
    ;; janitor
    ("o d" . denote-explore-identify-duplicate-notes)
    ("o z" . denote-explore-zero-keywords)
    ("o s" . denote-explore-single-keywords)
    ("o o" . denote-explore-sort-keywords)
    ("o w" . denote-explore-rename-keyword)
    ;; denote
    ("v n" . denote-explore-network)
    ("v v" . denote-explore-network-regenerate)
    ("v D" . denote-explore-degree-barchart)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bibliography & Citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :straight nil
  :demand t
  :init
  (defvar madmacs-library-bib-path (expand-file-name "library.bib" madmacs-library-path))
  
  (defvar-keymap madmacs-keymap-bib :doc "Keymap to manage bibliography notes")
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements madmacs-keymap-notes
      "b" `("Bib" . ,madmacs-keymap-bib))))

(use-package biblio)

(use-package ebib
  :bind
  (:map madmacs-keymap-bib
    ("b" . ebib))
  :custom
  (ebib-bibtex-dialect 'biblatex)
  (ebib-preload-bib-files (list madmacs-library-bib-path)))

(use-package bibtex
  :custom
  (bibtex-file-path madmacs-library-path))

(use-package citar
  :custom
  ;; set bibliography's location
  (citar-bibliography (list madmacs-library-bib-path))
  (citar-library-paths (list
                         (expand-file-name "Books" madmacs-library-path)
                         (expand-file-name "Papers" madmacs-library-path)))
  (citar-notes-paths (list (expand-file-name "Literature" madmacs-notes-path)))
  (citar-open-always-create-notes nil)
  (citar-file-note-extensions (list "org"))
  :bind
  (:map madmacs-keymap-bib
    ("c" . citar-create-note)))

(use-package citar-denote
  :after citar
  :custom
  (citar-denote-file-type 'org)
  (citar-denote-keyword "bib")
  (citar-denote-signature nil)
  (citar-denote-subdir "Literature") ; relative to denote dir
  (citar-denote-title-format "title")
  (citar-denote-title-format-andstr "and")
  (citar-denote-title-format-authors 1)
  (citar-denote-use-bib-keywords t)
  (citar-denote-template 'biblio)
  :bind
  (:map madmacs-keymap-bib
    ("d" . citar-denote-dwim)
    ("e" . citar-denote-open-reference-entry)
    ("a" . citar-denote-add-citekey)
    ("o" . citar-denote-open-note )
    ("k" . citar-denote-remove-citekey)
    ("r" . citar-denote-find-reference)
    ("l" . citar-denote-link-reference)
    ("f" . citar-denote-find-citation)
    ("x" . citar-denote-nocite)
    ("y" . citar-denote-cite-nocite)
    ("z" . citar-denote-nobib))

  :config
  (add-to-list 'denote-templates '(biblio . "* Abstract\n\n* Notes"))
  :init
  (citar-denote-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display what was written :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package DocView
  :straight (:type built-in))

(use-package esxml)

(use-package nov.el
  :after esxml
  :hook (after-init . setup-novel)
  :config
  (defun setup-novel (&rest _)
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))))

(provide 'madmacs-org-writing)
