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

(provide 'colemacs-treemacs)
