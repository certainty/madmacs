;; -*- lexical-binding: t; -*-

(use-package crux
  :bind
  (:map madmacs-mode-map
    ("C-<backspace>" . crux-kill-line-backwards)
    ("C-o" . crux-smart-open-line)
    ("M-o" . crux-smart-open-line-above)))

(use-package emacs
  :demand t
  :straight nil
  :hook
  (after-init . repeat-mode)
  :bind
  (:map madmacs-mode-map
	  ("M-z" . zap-up-to-char))
  :custom
  (next-line-add-newlines t)
  (kill-whole-line t)
  :config

  (defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))


(use-package expand-region
  :demand t)

(use-package expreg
  :bind
  ("C->" . expreg-expand)
  ("C-<" . expreg-contract)

  (:repeat-map madmacs-expreg-repeat-map
    (">" . expreg-expand)
    ("<" . expreg-contract)))

(use-package iedit
  :commands (iedit-mode iedit-dwim)
  :demand t
  :bind
  (:map madmacs-mode-map
    ("C-c '" . iedit-mode)
    ("C-c \"" . iedit-dwim))
  
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
      (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
            (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))

(use-package multiple-cursors)

(use-package embrace
  :demand t
  :bind
  (:map madmacs-mode-map
    ("C-," . embrace-commander))
  
  :init
  (add-hook 'markdown-mode-hook
    (lambda ()
      (embrace-add-pair ?_ "_" "_")
      (embrace-add-pair ?i "*" "*")
      (embrace-add-pair ?b "**" "**")))
 
  (defun embrace-double-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\" ))

  (defun embrace-single-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\')))

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)                 ; this is ok since it gives us xrefs as options
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
  :demand t
  :bind
  (:map goto-map
    ("c" . avy-goto-char-timer)
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


(provide 'madmacs-base-editing)
