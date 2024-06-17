(use-package dired
  :ensure nil
  :straight nil
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls")))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)

  :custom
  (dirvish-quick-access-entries         ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("w" "~/NewWork/Code/"                       "Work")
     ("p" "~/Private/"        "Private")))

  (dirvish-use-header-line 'global)
  (dirvish-default-layout '(0 0.3 0.7))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))

  (dirvish-attributes
   '(vc-state subtree-state all-the-icons file-time file-size collapse git-msg ))

  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;;(dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'

  :bind                                 ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  ((:map madmacs-open-map
         ("p" . dirvish-dwim)
         ("P" . dirvish-side))

   (:map madmacs-file-map
         ("d" . dirvish))

   :map dirvish-mode-map                ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump)       ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)          ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)            ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-j" . dirvish-fd-jump)))


(use-package wdired
  :ensure nil
  :straight nil
  :commands (wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook dired-mode)

(provide 'madmacs-files-dired)
