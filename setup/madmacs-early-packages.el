;; -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (package-enable-at-startup nil)
  (load-prefer-newer t)
  (debug-on-error madmacs-debug)
  (byte-compile-warnings '(not free-vars docstrings unresolved noruntime lexical make-local obsolete)))

(provide 'madmacs-early-packages)
