;; -*- lexical-binding: t; -*-

(use-package emacs
  :demand t
  :straight nil
  :init
  (add-to-list 'auth-sources "~/.config/authinfo.gpg" t))

(use-package auth-source-1password
  :init
  (auth-source-1password-enable))

(use-package pass
  :after epa
  :commands (pass)
  :init
  (auth-source-pass-enable))

(use-package epa
  :straight (:type built-in)
  :config
  (setq epg-pinentry-mode 'loopback))

(provide 'madmacs-tools-pass)
