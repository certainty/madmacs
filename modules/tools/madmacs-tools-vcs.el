(use-package magit
  :ensure t
  :bind
  (:map madmacs-vcs-map
        ("v" . magit-status)
        ("t" . magit-todos-list)
        ("d" . magit-diff-buffer-file))
  :custom
  (git-commit-summary-max-length 80)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (magit-diff-refine-hunk t)
  :config
  (setopt magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (add-to-list 'display-buffer-alist
             '("\\magit:"
               (display-buffer-same-window))))

(use-package git-timemachine
  :ensure t
  :bind
  (:map madmacs-toggle-map
    ("v" . git-timemachine-toggle)))

(use-package hydra
  :ensure t)

(use-package smerge-mode
  :ensure nil
  :straight nil
  :after (hydra general)
  :config
  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower )
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))


(provide 'madmacs-tools-vcs)
