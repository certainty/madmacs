;; -*- lexical-binding: t; -*-

;; nope you need a modern emacs
(when (version< emacs-version "29")
  (error "Madmacs requires Emacs 29 or later"))

;; speed up startup
(defvar madmacs--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)
            (setq file-name-handler-alist madmacs--file-name-handler-alist)))



;; setup the load paths so that I can require my own packages to drive the boot process
(dolist (dir (directory-files (expand-file-name "config" user-emacs-directory) t "^[^.]+$"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(require 'madmacs-settings)
(require 'madmacs-runlevels)
(require 'madmacs-boot)

(provide 'madmacs-bootstrap)
