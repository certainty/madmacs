(use-package docker
  :ensure t
  :commands (docker))

(use-package dockerfile-ts-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'madmacs-tools-docker)
