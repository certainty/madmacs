;; -*- lexical-binding: t; -*-
;; edit actions
;; this is a collection of actions that can be used to manipulate text in the buffer

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)                 ; this is ok since it gives us xrefs as options
   ("M-." . embark-dwim)                ; this is also ok since it also gives us xrefs as options
   ("C-h B" . embark-bindings))
  
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; by default we use completing read instead of which-key like selection
  (setopt embark-prompter 'embark-completing-read-prompter)

  (setq embark-indicators
        '(embark-highlight-indicator
           embark-isearch-highlight-indicator)))


(use-package avy
  :ensure t
  :demand t
  :bind
  (:map goto-map
    ("c" . avy-goto-char-time)
    ("C" . avy-goto-char))

  :config
  ;; https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  ;; this is wrong word
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?m  avy-dispatch-alist) 'avy-action-mark-to-char)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange)))


;; iedit / edit multiple regions
;; (use-package iedit
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c '") 'iedit-mode)
;;   (global-set-key (kbd "C-c \"") 'iedit-dwim)
  
;;   (defun iedit-dwim (arg)
;;     "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;;     (interactive "P")
;;     (if arg
;;       (iedit-mode)
;;       (save-excursion
;;         (save-restriction
;;           (widen)
;;           ;; this function determines the scope of `iedit-start'.
;;           (if iedit-mode
;;             (iedit-done)
;;             ;; `current-word' can of course be replaced by other
;;             ;; functions.
;;             (narrow-to-defun)
;;             (iedit-start (current-word) (point-min) (point-max))))))))


(provide 'madmacs-edit-actions)
