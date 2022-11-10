(use-package format-all)
(use-package reveal-in-osx-finder)
(use-package buffer-move)
(use-package restart-emacs)

(use-package hl-todo
  :custom
  (hl-todo-color-background t)
  :init
  (global-hl-todo-mode 1))

(use-package org-tree-slide
  :custom
  (org-tree-slide-slide-in-effect nil))

(use-package emojify
  :custom
  (emojify-display-style 'image)
  :hook (after-init . global-emojify-mode))

(use-package avy
  :config
  (setq avy-background t))

;; hydras ====================================================

(use-package hydra)

(defhydra cole/present ()
  "presentation"
  ("t" org-tree-slide-mode "toggle slide mode")
  ("n" org-tree-slide-move-next-tree "next")
  ("p" org-tree-slide-move-previous-tree "previous")
  ("q" nil "quit" :exit t)) ;; how to add another "org-tree-slide-mode 0" to turn it off when quitting the hydra

(cole/leader-keys
  "gt" '(cole/git-timemachine "time machine"))

(defhydra cole/git-timemachine ()
  "git-timemachine"
  ("j" git-timemachine-show-next-revision "next revision")
  ("k" git-timemachine-show-previous-revision "previous revision")
  ("c" git-timemachine-show-commit "show commit")
  ("y" git-timemachine-kill-abbreviated-revision "yank short hash")
  ("q" git-timemachine-quit "quit" :exit t))

(cole/leader-keys
  "ts" '(cole/scale-text :which-key "scale text"))

(defhydra cole/scale-text (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(cole/leader-keys
  "wr" '(cole/resize-window :which-key "resize window"))

(defhydra cole/resize-window ()
  "resize windows"
  ("j" evil-window-increase-height "increase height")
  ("k" evil-window-decrease-height "decrease height")
  ("l" evil-window-increase-width "increase width")
  ("h" evil-window-decrease-width "decrease width")
  ("q" nil "quit" :exit t))

(provide 'colemacs-applications)
