;; -*- lexical-binding: t; -*-

(use-package docker
  :commands (docker))

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'")

(provide 'madmacs-tools-docker)
