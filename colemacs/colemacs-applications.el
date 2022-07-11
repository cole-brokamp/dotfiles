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

;; treemacs --------------------------------------------------------------------------
(use-package treemacs
  :defer t
  :custom
  (treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  (treemacs-hide-gitignored-files-mode nil))

;; TODO (why) does this wreck using ? to summon hydra ...?
;; (use-package treemacs-evil
;;   :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package avy
  :config
  (setq avy-background t))
(cole/leader-keys
  "sa" '(evil-avy-goto-char-timer :which-key "avy"))

;; spell checking
(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (cole/leader-keys
    "S" '(:ignore t :which-key "spelling")
    "Ss" '(flyspell-mode :which-key "toggle spelling mode")
    "Sp" '(flyspell-prog-mode :which-key "enable spelling prog mode")
    "Sb" '(flyspell-buffer :which-key "spell check buffer")
    "Sc" '(flyspell-correct-wrapper :which-key "correct word"))
  (add-hook 'flyspell-prog-mode-hook (lambda () (message "Flyspell prog-mode enabled in current buffer"))))

;; hydras ====================================================

(use-package hydra)

(defhydra cole/present ()
  "presentation"
  ("t" org-tree-slide-mode "toggle slide mode")
  ("n" org-tree-slide-move-next-tree "next")
  ("p" org-tree-slide-move-previous-tree "previous")
  ("q" nil "quit" :exit t)) ;; how to add another "org-tree-slide-mode 0" to turn it off when quitting the hydra

(defhydra cole/git-timemachine ()
  "git-timemachine"
  ("j" git-timemachine-show-next-revision "next revision")
  ("k" git-timemachine-show-previous-revision "previous revision")
  ("c" git-timemachine-show-commit "show commit")
  ("y" git-timemachine-kill-abbreviated-revision "yank short hash")
  ("q" git-timemachine-quit "quit" :exit t))

(defhydra cole/scale-text (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(defhydra cole/resize-window ()
  "resize windows"
  ("j" evil-window-increase-height "increase height")
  ("k" evil-window-decrease-height "decrease height")
  ("l" evil-window-increase-width "increase width")
  ("h" evil-window-decrease-width "decrease width")
  ("q" nil "quit" :exit t))
  

(cole/leader-keys
  "ts" '(cole/scale-text/body :which-key "scale text")
  "wr" '(cole/resize-window/body :which-key "resize window"))

(provide 'colemacs-applications)
