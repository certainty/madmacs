;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :custom
  (add-to-list 'auth-sources "~/.config/authinfo.gpg" t))

(use-package pass
  :ensure t
  :after epa)

(use-package epa
  :ensure nil
  :straight nil
  :config
  (setq epg-pinentry-mode 'loopback))

(provide 'madmacs-tools-pass)
