;; -*- lexical-binding: t; -*-

(defgroup madmacs nil
  "Madmacs configuration group"
  :prefix "madmacs-"
  :group 'emacs)

(defcustom madmacs-font "Iosevka Nerd Font"
  "Fixed pitch font"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-variable-pitch-font "Iosevka Aile"
  "Variable pitch font"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-font-size 130
    "Default font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-presentation-font-size 190
    "Presentation font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-presentation-bigger-font-size 210
    "Bigger presentation font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-user-full-name ""
  "User full name"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-enable-meow nil
  "Enable meow mode"
  :type 'boolean
  :group 'madmacs)

(defcustom madmacs-user-email ""
  "User email"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-shared-silo-path "~/Documents/Silos/Shared"
  "The base path to the shared silo for documents, org, files and other shared resources"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-private-silo-path "~/Documents/Silos/Strictly Private"
  "The base path to the private silo for documents, org, files and other shared resources"
  :type 'string
  :group 'madmacs)

(use-package emacs
  :custom
  (user-full-name madmacs-user-full-name)
  (user-mail-address madmacs-user-email)
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(provide 'madmacs-base-settings)

