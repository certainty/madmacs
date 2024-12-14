;; -*- lexical-binding: t; -*-

;;; This adds vanilla emacs key support
;;; While you may think there is not much to do there are just a couple of things that make
;;; the vanilla emacs keybindings a bit more comfortable and easier to use.


(use-package repeat
  :ensure t
  :straight nil
  :custom
  (repeat-exit-timeout 3)

  :config
  (dolist (command '(next-error previous-error))
    (put command 'repeat-exit-timeout 'no))
  
  :bind
  (:map goto-map
    ("]" . next-buffer)
    ("[" . previous-buffer))

  (:repeat-map madmacs-buffer-repeat-map
    ("]" . next-buffer)
    ("[" . previous-buffer))
  
  ;; Object-specific Motion Repeat 
  (:repeat-map madmacs-line-repeat-map
    ("^" . join-line))

  (:repeat-map madmacs-paragraph-repeat-map
    ("}" . forward-paragraph)
    ("{" . backward-paragraph))
  
  (:repeat-map madmacs-undo-repeat-map
    ("u" . undo))

  (:repeat-map madmacs-scroll-repeat-map
    ("v" . scroll-up-command)
    ("M-v" . scroll-down-command)
    ("l" . recenter-top-bottom))

  (:repeat-map madmacs-windows-repeat-map
    ("o" . other-window))
  
  :init
  (defun madmacs--repeatize (keymap)
    "Add `repeat-mode' support to an existing KEYMAP.
    If you define the keymap new via defvar-keymap use the :repeat property instead"
    (map-keymap
      (lambda (_key cmd)
        (when (symbolp cmd)
          (put cmd 'repeat-map keymap)))
      (symbol-value keymap))))




(provide 'madmacs-keys-emacs)
