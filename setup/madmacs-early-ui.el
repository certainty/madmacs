;; -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (frame-inhibit-implied-resize nil)
  (frame-title-format "\n")
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-splash-screen t)
  (inhibit-startup-echo-area-message "david.krentzlin")
  (initial-scratch-message nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (initial-major-mode 'fundamental-mode))

(provide 'madmacs-early-ui)
