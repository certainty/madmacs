;; -*- lexical-binding: t; -*-



;;; This adds vanilla emacs key support
;;; While you may think there is not much to do there are just a couple of things that make
;;; the vanilla emacs keybindings a bit more comfortable and easier to use.


(use-package repeat
  :ensure t
  :straight nil
  :bind

  (:repeat-map madmacs-motion-repeat-map
    ("f" . forward-char)
    ("b" . backward-char)
    
    ("n" . next-line)
    ("p" . previous-line))
  
  (:repeat-map madmacs-kill-repeat-map
    ("k" . kill-whole-line)
    ("b" . backward-kill-word)
    ("d" . delete-char))
  
  (:repeat-map madmacs-undo-repeat-map
    ("u" . undo))

  (:repeat-map madmacs-search-repeat-map
    ("s" . isearch-forward)
    ("r" . isearch-backward))
  
  :init

  (defun madmacs--repeatize (keymap)
    "Add `repeat-mode' support to an existing KEYMAP.
    If you define the keymap new via defvar-keymap use the :repeat property instead"
    (map-keymap
      (lambda (_key cmd)
        (when (symbolp cmd)
          (put cmd 'repeat-map keymap)))
      (symbol-value keymap)))

  )


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
