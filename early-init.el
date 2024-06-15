;;;; -*- lexical-binding: t; -*-

;; speed up startup
(defvar macmacs-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; setup the load paths so that I can require my own packages to drive the boot process
(dolist (dir (directory-files (expand-file-name "config" user-emacs-directory) t "^[^.]+$"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(require 'madmacs-boot)
(require 'madmacs-runlevels)

(madmacs--pre-boot)
