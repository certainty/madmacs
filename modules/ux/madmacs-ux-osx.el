;; -*- lexical-binding: t; -*-

(use-package emacs
  :config
  (when madmacs--sys-mac-p
   (setopt mac-option-modifier 'meta)
   (setopt mac-command-modifier 'super)
   (setopt mac-function-modifier 'hyper)
   (setopt mac-right-option-modifier 'none)
   ;(setopt ns-use-native-fullscreen t)
   (setopt ns-use-thin-smoothing t)
   (setopt ns-pop-up-frames nil)
   (setopt trash-directory "~/.Trash")
   (setopt mac-allow-anti-aliasing t)
   (setopt select-enable-clipboard t)

   (with-eval-after-load 'auth-source
     (add-to-list 'auth-sources 'macos-keychain-internet)
     (add-to-list 'auth-sources 'macos-keychain-generic))))

(use-package reveal-in-osx-finder
  :if madmacs--sys-mac-p)

(use-package osx-lib
  :if madmacs--sys-mac-p)

(provide 'madmacs-ux-osx)
