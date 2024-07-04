;; -*- lexical-binding: t; -*-

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)

  (setq evil-normal-state-tag   (propertize " Ⓝ " 'face '((:weight extrabold :foreground "#ffffff")))
        evil-emacs-state-tag    (propertize " Ⓔ " 'face '((:weight extrabold :foreground "#c0965b")))
        evil-insert-state-tag   (propertize " Ⓘ " 'face '((:weight extrabold :foreground "#ff66ff")))
        evil-motion-state-tag   (propertize " Ⓜ " 'face '((:weight extrabold :foreground "#79a8ff")))
        evil-visual-state-tag   (propertize " Ⓥ " 'face '((:weight extrabold :foreground "#d0bc00")))
        evil-operator-state-tag (propertize " Ⓞ " 'face '((:weight extrabold :foreground "#f78fe7")))))

;; (use-package doom-modeline
;;   :ensure t
;;   :custom
;;   (doom-modeline-major-mode-icon nil)
;;   (doom-modeline-time nil)
;;   (doom-modeline-check-simple-format t)

;;   :config
;;   (doom-modeline-mode))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(provide 'madmacs-ui-modeline)
