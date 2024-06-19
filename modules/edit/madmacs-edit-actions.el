(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act) ; this is ok since it gives us xrefs as options
   ("M-." . embark-dwim) ; this is also ok since it also gives us xrefs as options
   ("C-h B" . embark-bindings)
   (:map madmacs-actions-map
         ("a" . embark-act)
         ("d" . embark-dwim)))
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
  
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :after consult
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)
   (:map madmacs-goto-map
         ("j" . avy-goto-char-timer)
         ("J" . avy-goto-char-2)))
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


(provide 'madmacs-edit-actions)
