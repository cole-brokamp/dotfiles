(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (when (file-directory-p "~/icloud/works")
      (setq projectile-project-search-path '("~/icloud/works")))
  (setq projectile-switch-project-action #'projectile-dired)
  (cole/leader-keys
    "p" '(:ignore t :which-key "projects")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pd" '(projectile-kill-buffers :which-key "close project buffers")
    "ps" '(projectile-ag :which-key "search in project files")
    "pr" '(projectile-replace :which-key "replace in project files")
    "pl" '(:ignore t :which-key "TODO - open project in new layout")
  ))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  :custom
  (eyebrowse-new-workspace t)
  :init
  (cole/leader-keys
    "l" '(:ignore t :which-key "layouts")
    "l TAB" '(eyebrowse-last-window-config :which-key "last layout")
    "lc" '(eyebrowse-create-named-window-config :which-key "create layout")
    "lx" '(eyebrowse-close-window-config :which-key "close layout")
    "lr" '(eyebrowse-rename-window-config :which-key "rename layout")
    "ll" '(eyebrowse-switch-to-window-config :which-key "switch to layout")
    "l C-h" '(eyebrowse-prev-window-config :which-key "previous layout")
    "l C-l" '(eyebrowse-next-window-config :which-key "next layout")
    ))

(provide 'colemacs-layouts)
