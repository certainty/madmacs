;; -*- lexical-binding: t; -*-

(use-package google-translate
  :ensure t
  :after embark
  :config
  (keymap-set embark-region-map "t" 'google-translate-at-point)

  (setq google-translate-default-target-language "en")
  (setq google-translate-backend-method 'curl)
  (setq google-translate-listen-program "mpv")
  (setq google-translate-output-destination 'echo-area)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-enable-ido-completion t)
  (setq google-translate-show-phonetic t)
  (setq google-translate-phonetic-fontify-function 'google-translate-fontify-buffer)
  (setq google-translate-translation-directions-alist
    '(("de" . "en") ("en" . "de")
       ("de" . "fr") ("fr" . "de")
       ("en" . "is") ("is" . "en"))))

(provide 'madmacs-tools-translate)
