(use-package magit
  :custom
  ((vc-follow-symlinks t))
  :config
  (cole/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gc" '(magit-clone :which-key "git clone")
    "gf" '(magit-find-file :which-key "find file")
  ))

(use-package forge)

(use-package git-timemachine
  :config
  (cole/leader-keys
    "gt" '(cole/git-timemachine/body :which-key "time machine")
    ))

(use-package github-browse-file
  :config
  (cole/leader-keys
    "gh" '(github-browse-file :which-key "browse on github")
    ))

(provide 'colemacs-git)
