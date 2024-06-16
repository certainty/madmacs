(use-package embark
  :ensure t
  :bind
  (:map madmacs-actions-map
        ("a" . embark-act)))

(use-package embark-consult
  :ensure t
  :after consult
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (("C-c ," . avy-goto-char)
   (:map madmacs-goto-map
         ("j" . avy-goto-char-timer)
         ("J" . avy-goto-char-2)
         ("l" . avy-goto-line)
         ("w" . avy-goto-word-1)
         ("W" . avy-goto-word-0)
         ("C" . avy-goto-char))))

(provide 'madmacs-edit-actions)
