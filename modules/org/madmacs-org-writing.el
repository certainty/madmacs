;; -*- lexical-binding: t; -*-

(use-package emacs
  :straight nil
  :demand t
  :custom
  (dictionary-server "dict.org")

  :init
  (defvar madmacs-notes-shared-vault-path (file-truename "~/Vault/Silos/Shared"))
  (defvar madmacs-notes-private-vault-path (file-truename "~/Vault/Silos/Strictly Private"))

  (defvar-keymap madmacs-keymap-notes :doc "Writing and note taking")
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements madmacs-keymap-global
      "n" `("Notes+Writing" . ,madmacs-keymap-notes))))

(use-package flyspell
  :straight nil
  :hook
  (text-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode)
  :bind
  (:map madmacs-keymap-global
    ("." . flyspell-correct-word))
  
  :config
  ;; don't shadow embark or avy bindings
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
  :demand t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map madmacs-keymap-global
    ("C" . denote)
    ("m" . madmacs-notes-meeting-new-recurring))
  
  (:map madmacs-keymap-notes
    ("n" . denote)
    ("N" . denote-subdirectory)
    ("d" . denote-date)
    ("r" . denote-region) ; "contents" mnemonic

    ("i" . denote-link) ; "insert" mnemonic
    ("t" . denote-type)
    ("z" . denote-signature) ; "zettelkasten" mnemonic
    ("l" . denote-add-links)
    ("b" . denote-backlinks)
    
    ("f f" . denote-find-link)
    ("f b" . denote-find-backlink)
    
    ("m" . denote-rename-file)
    ("M" . denote-rename-file-using-front-matter))

  (:map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory madmacs-notes-shared-vault-path)
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
  (denote-dired-directories (list denote-directory (concat org-directory "/.attachments")))

  (denote-templates
    '((project . "* Objective\n\n* References\n")
      (meeting . "* Outcome\n\n* Notes\n\n* Tasks\n** Our\n ** Theirs\n\n" )
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
             (let ((denote-use-directory (expand-file-name "/Resources/Meetings" (denote-directory)))
                    (denote-use-keywords (list "meeting"))
                    (denote-use-template 'meeting))
               (denote-org-capture))))
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)
      org-capture-templates))

  :config
  (require 'denote-org-extras)
  (require 'denote-silo-extras)
  (denote-rename-buffer-mode 1)

  ;;; Special support for meeting minutes
  (defvar madmacs-notes-meeting-directory "/Resources/Meetings"
    "The directory for meeting minutes")
  
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
      (user-error "No files for recurring meeting with name `%s'" name))))

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

(use-package consult-notes
  :after org
  :bind
  (:map goto-map
    ("n n" . consult-notes))
  (:map search-map
    ("n g" . consult-notes-search-in-all-notes))

  :custom
  (consult-notes-file-dir-sources
    `(("Org"       ?o "~/Org")
       ("Shared"   ?n ,madmacs-notes-shared-vault-path)
       ("Private"  ?p ,madmacs-notes-private-vault-path)))
  :config
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bibliography & Citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :straight nil
  :demand t
  :init
  (defvar madmacs-notes-bib-path (concat madmacs-notes-shared-vault-path "/Resources/Library/library.bib"))
  (defvar madmacs-notes-library-path (concat madmacs-notes-shared-vault-path "/Resources/Library"))
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
  (ebib-preload-bib-files (list madmacs-notes-bib-path)))

(use-package bibtex
  :custom
  (bibtex-file-path madmacs-notes-library-path))

(use-package citar
  :demand t
  :custom
  ;; set bibliography's location
  (citar-bibliography (list madmacs-notes-bib-path))
  (citar-library-paths (list (concat madmacs-notes-library-path "/Books") (concat madmacs-notes-library-path "/Papers")))
  (citar-notes-paths (list madmacs-notes-library-path))
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
  (citar-denote-subdir "/Resources/Library") ; relative to denote dir
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
