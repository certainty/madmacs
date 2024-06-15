(defvar bootstrap-version)
(setopt straight-base-dir madmacs--data-dir)

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
  :custom
  (straight-use-package 'use-package)
  (straight-use-package-by-default t))

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-verbose t)
  (use-package-minmum-reported-time 0)
  (use-package-expand-minimally nil)
  (package-user-dir (madmacs--ensure-data-dir "elpa"))
  (package-archives
   '(("elpa" . "https://elpa.gnu.org/packages/")
     ("elpa-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

  (package-archive-priority
   '(("elpa-devel" . 99)
     ("melpa" . 90))))

(use-package no-littering
  :ensure t
  :demand t
  :init
  (setopt no-littering-etc-directory (madmacs--ensure-data-dir "etc"))
  (setopt no-littering-var-directory (madmacs--ensure-data-dir "var")))

(use-package gnutls
  :ensure nil
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

(use-package auto-compile
  :ensure t
  :hook (emacs-startup . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode))

(provide 'madmacs-init-packages)
