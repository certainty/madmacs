;; -*- lexical-binding: t; -*-

;;;
;;; Madmacs
;;;

(when (< emacs-major-version 29)
  (error "Emacs 29 or greater required"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup package manager & use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :init
  (setopt native-comp-speed 2)
  (setopt native-comp-jit-compilation t) ;; async compilation
  (setopt package-enable-at-startup nil)
  (setopt load-prefer-newer t))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :demand t
  :custom
  (straight-check-for-modifications '(check-on-save))
  (straight-use-package 'use-package)
  (straight-use-package-by-default t))

(use-package use-package
  :demand t
  :custom
  (use-package-always-ensure t)
  (use-package-verbose t)
  (use-package-minmum-reported-time 0)
  (use-package-expand-minimally nil)
  (package-archives
   '(("elpa" . "https://elpa.gnu.org/packages/")
     ("elpa-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

  (package-archive-priority
   '(("elpa-devel" . 99)
     ("melpa" . 90))))

(use-package emacs
  :init
  (add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t))))

;; secure connections
(use-package gnutls
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;; don't pollute the emacs directory with package files
(use-package no-littering
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl-lib
  :straight nil
  :demand t)

(use-package subr-x
  :straight nil
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Madmacs main config starts here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Setup additional load paths so that we can load our modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(dolist (dir (directory-files (expand-file-name "modules" user-emacs-directory) t "^[^.]+$"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;;; Define a minor mode which we load in all files.
;;; This allows me to override keybindings more predictibly
(use-package emacs
  :hook
  (find-file . madmacs-mode)
  
  :init
  (define-minor-mode madmacs-mode
    "Minor mode for madmacs, which gives a home for my customizations and keybindings."
    :global t
    :lighter " #"
    :keymap (make-sparse-keymap)))

;;; Now we can load the madmacs modules
(require 'madmacs-settings)
(require 'madmacs-keys)

(when (eq system-type 'darwin)
  (require 'madmacs-osx))

(require 'madmacs-ui)
(require 'madmacs-display-elements)
(require 'madmacs-completion)
(require 'madmacs-files)
(require 'madmacs-search)
(require 'madmacs-editing)
(require 'madmacs-workspaces)
(require 'madmacs-dashboard)

