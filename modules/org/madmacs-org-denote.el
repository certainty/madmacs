;; -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map madmacs-keymap-global
    ("d n" . denote)
    ("d c" . denote-region) ; "contents" mnemonic
    ("d N" . denote-type)
    ("d d" . denote-date)
    ("d z" . denote-signature) ; "zettelkasten" mnemonic
    ("d s" . denote-subdirectory)
    ("d t" . denote-journal-extras-new-or-existing-entry)
    ("d i" . denote-link) ; "insert" mnemonic
    ("d I" . denote-add-links)
    ("d b" . denote-backlinks)
    ("d f f" . denote-find-link)
    ("d f b" . denote-find-backlink)
    ("d r" . denote-rename-file)
    ("d R" . denote-rename-file-using-front-matter))
    
  (:map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory (file-truename "~/org/denote"))
  (denote-save-buffers nil)
  (denote-known-keywords '("nw1" "messaging" "xws" "pmd" "cloud_migration" "emacs" "health" "finance" "home" "family" "dea" "contact"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  
  ;; journal
  (denote-journal-extras-directory (file-truename "~/org/denote/journal"))
  (denote-journal-extras-keyword "journal")
  (denote-journal-extras-title-format 'day-date-month-year)

  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%i\n\n%?")
    (add-to-list 'org-capture-templates
      '("N" "New note (with denote.el)" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save nil
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured nil)))

  :config
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  ;;(require 'denote-journal-extras)
  (setopt denote-dired-directories (list denote-directory (file-truename "~/org/.attachments")))
  (denote-rename-buffer-mode 1))

;; TODO: use https://github.com/mclear-tools/consult-notes instead as it is more versatile
(use-package consult-denote
  :ensure t
  :after (consult denote)
  :init
  (consult-denote-mode 1))

(provide 'madmacs-org-denote)
