 ;; -*- lexical-binding: t; -*-

(use-package emacs
  :init
  (setopt mac-option-modifier 'meta)
  (setopt mac-command-modifier 'super)
  (setopt mac-function-modifier 'hyper)
  (setopt mac-right-option-modifier 'none)

  (setopt ns-use-thin-smoothing t)
  (setopt ns-pop-up-frames nil)
  (setopt trash-directory "~/.Trash")
  (setopt mac-allow-anti-aliasing t)
  (setopt select-enable-clipboard t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (with-eval-after-load 'auth-source
    (add-to-list 'auth-sources 'macos-keychain-internet)
    (add-to-list 'auth-sources 'macos-keychain-generic)))

(use-package reveal-in-osx-finder)
(use-package osx-lib)

(provide 'madmacs-base-osx)
