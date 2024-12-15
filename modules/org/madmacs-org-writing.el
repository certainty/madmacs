;; -*- lexical-binding: t; -*-

(defvar-keymap madmacs-keymap-notes :doc "Writing and note taking")
(which-key-add-keymap-based-replacements madmacs-keymap-global
  "n" `("Notes+Writing" . ,madmacs-keymap-notes))

(use-package emacs
  :straight nil
  :demand t
  :custom
  (dictionary-server "dict.org"))

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
  :config
  (keymap-set embark-region-map "t" 'google-translate-at-point)

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
             '(("m" "Meeting")
               ("mm" "Adhoc" entry (file+olp+datetree "meetings.org" "Adhoc")
                "* %T %?n
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
                :tree-type week))))))

;; Permanent notes
(use-package denote
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
      '("nn" "Permanent Note" plain
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
  :after org
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


(provide 'madmacs-org-writing)
