;; -*- lexical-binding: t; -*-

(load (expand-file-name "setup/madmacs-bootstrap" user-emacs-directory) nil t)

;; Tweak some very basic settings
;(setopt madmacs-debug t)
(madmacs--setup-env)
(madmacs--boot-early)
