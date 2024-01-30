(cole/leader-keys
  "s" '(:ignore t :which-key "search")
  "ss" '(swiper :which-key "swiper")
  "sb" '(swiper-all :which-key "swiper all buffers")
  "sr" '(query-replace :which-key "search and replace")
  "sR" '(query-replace-regexp :which-key "search and replace (regex)")
  "sp" '(projectile-ag :which-key "search in project files")
  "j" '(:ignore t :which-key "jump")
  "jb" '(evil-jump-backward :which-key "back")
  "jf" '(evil-jump-forward :which-key "forward")
  "jj" '(evil-avy-goto-char-timer :which-key "to char")
  "jl" '(evil-avy-goto-line :which-key "to line")
  )


(use-package avy
  :config
  (setq avy-background t))


(provide 'colemacs-jump-search)
