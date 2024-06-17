(use-package docker
  :ensure t
  :bind
  (:map madmacs-open-map
        ("D" . docker)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'madmacs-tools-docker)
