(use-package docker
  :ensure t
  :commands (docker))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'madmacs-tools-docker)
