(use-package hl-todo
  :custom
  (hl-todo-color-background t)
  :init
  (global-hl-todo-mode 1))

(use-package org-tree-slide
  :custom
  (org-tree-slide-slide-in-effect nil))

(use-package avy
  :config
  (setq avy-background t))

;; hydras ====================================================


(defhydra cole/present ()
  "presentation"
  ("t" org-tree-slide-mode "toggle slide mode")
  ("n" org-tree-slide-move-next-tree "next")
  ("p" org-tree-slide-move-previous-tree "previous")
  ("q" nil "quit" :exit t)) ;; how to add another "org-tree-slide-mode 0" to turn it off when quitting the hydra

(cole/leader-keys
  "ts" '((lambda () (interactive) (cole/scale-text/body)) :which-key "scale text"))

(defhydra cole/scale-text (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))


(provide 'colemacs-applications)
