(cole/leader-keys
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "git status")
  "gc" '(magit-clone :which-key "git clone")
  "gf" '(magit-find-file :which-key "find file")
  "gt" '(cole/git-timemachine/body :which-key "time machine")
  ;; "gt" '(cole/git-timemachine "time machine")
  "gh" '(github-browse-file :which-key "browse on github")
  )

(use-package magit
  :custom
  ((vc-follow-symlinks t))
  )

(use-package forge)

(use-package git-timemachine)

(use-package github-browse-file)


(defhydra cole/git-timemachine ()
  "git-timemachine"
  ("j" git-timemachine-show-next-revision "next revision")
  ("k" git-timemachine-show-previous-revision "previous revision")
  ("c" git-timemachine-show-commit "show commit")
  ("y" git-timemachine-kill-abbreviated-revision "yank short hash")
  ("q" git-timemachine-quit "quit" :exit t))

(provide 'colemacs-git)
