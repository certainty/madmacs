;; -*- lexical-binding: t; -*-

;; nope you need a modern emacs
(when (version< emacs-version "29")
  (error "Madmacs requires Emacs 29 or later"))

;; speed up startup
(defvar madmacs--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq madmacs--original-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold madmacs--original-cons-threshold)
            (setq file-name-handler-alist madmacs--file-name-handler-alist)))

;; setup the load paths so that I can require my own packages to drive the boot process
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(dolist (dir (directory-files (expand-file-name "modules" user-emacs-directory) t "^[^.]+$"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(require 'madmacs-settings)
(require 'madmacs-boot)

(provide 'madmacs-bootstrap)
