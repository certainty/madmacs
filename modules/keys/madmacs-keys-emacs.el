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
    ("C-n" . next-buffer)
    ("C-p" . previous-buffer))

  (:repeat-map madmacs-buffer-repeat-map
    ("n" . next-buffer)
    ("p" . previous-buffer))
  
  ;; Object-specific Motion Repeat Maps
  (:repeat-map madmacs-char-repeat-map
    ("f" . forward-char)
    ("b" . backward-char)
    ("d" . delete-char))

  (:repeat-map madmacs-word-repeat-map
    ("f" . forward-word)
    ("b" . backward-word)
    ("d" . kill-word)
    ("DEL" . backward-kill-word))
  
  (:repeat-map madmacs-sentence-repeat-map
    ("f" . forward-sentence)
    ("b" . backward-sentence))

  (:repeat-map madmacs-sexp-repeat-map
    ("f" . forward-sexp)
    ("b" . backward-sexp)
    ("k" . kill-sexp))
  
  (:repeat-map madmacs-line-repeat-map
    ("n" . next-line)
    ("p" . previous-line)
    ("a" . beginning-of-line)
    ("e" . end-of-line)
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


(use-package embrace
  :ensure t
  :demand t
  :bind
  (("C-c ca" . embrace-add)
    ("C-c cc" . embrace-change)
    ("C-c cd" . embrace-delete))
  :init
  (add-hook 'markdown-mode-hook
    (lambda ()
      (embrace-add-pair ?_ "_" "_")
      (embrace-add-pair ?i "*" "*")
      (embrace-add-pair ?b "**" "**")))

  (defun embrace-double-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\"))

  (defun embrace-single-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\')))

(provide 'madmacs-keys-emacs)
