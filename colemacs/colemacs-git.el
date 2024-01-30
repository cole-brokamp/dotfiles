(cole/leader-keys
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "git status")
  "gc" '(magit-clone :which-key "git clone")
  "gh" '(github-browse-file :which-key "browse on github")
  )
(use-package magit
  :custom
  ((vc-follow-symlinks t))
  )
(use-package github-browse-file)

(provide 'colemacs-git)
