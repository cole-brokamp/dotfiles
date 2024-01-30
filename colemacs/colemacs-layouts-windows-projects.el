(cole/leader-keys
  "p" '(:ignore t :which-key "projects")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pd" '(projectile-kill-buffers :which-key "close project buffers")
  "ps" '(projectile-ag :which-key "search in project files")
  "pr" '(projectile-replace :which-key "replace in project files")
  "pl" '(:ignore t :which-key "TODO - open project in new layout")
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (when (file-directory-p "~/icloud/works")
    (setq projectile-project-search-path '("~/icloud/works")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


(cole/leader-keys
  "l" '(:ignore t :which-key "layouts")
  "l TAB" '(eyebrowse-last-window-config :which-key "last layout")
  "lc" '(eyebrowse-create-named-window-config :which-key "create layout")
  "lx" '(eyebrowse-close-window-config :which-key "close layout")
  "lr" '(eyebrowse-rename-window-config :which-key "rename layout")
  "ll" '(eyebrowse-switch-to-window-config :which-key "switch to layout")
  )

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  :custom
  (eyebrowse-new-workspace t)
  :init)

(cole/leader-keys
  "w" '(:ignore t :which-key "windows")
  "w-" '(cole/split-window-below-and-focus :which-key "split down")
  "w/" '(cole/split-window-right-and-focus :which-key "split right")
  "w=" '(balance-windows-area :which-key "equal window areas")
  "wH" '(evil-window-move-far-left :which-key "move left")
  "wJ" '(evil-window-move-very-bottom :which-key "move down")
  "wK" '(evil-window-move-very-top :which-key "move up")
  "wL" '(evil-window-move-far-right :which-key "move right")
  "wd" '(evil-window-delete :which-key "delete window")
  "wf" '(make-frame :which-key "make into frame")
  "wh" '(evil-window-left :which-key "focus left")
  "wj" '(evil-window-down :which-key "focus down")
  "wk" '(evil-window-up :which-key "focus up")
  "wl" '(evil-window-right :which-key "focus right")
  "wm" '(cole/toggle-maximize-buffer :which-key "maximize window")
  "wr" '((lambda () (interactive) (cole/resize-window/body)) :which-key "resize window")
)

(defun cole/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun cole/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun cole/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))

(defhydra cole/resize-window ()
  "resize windows"
  ("j" evil-window-increase-height "increase height")
  ("k" evil-window-decrease-height "decrease height")
  ("l" evil-window-increase-width "increase width")
  ("h" evil-window-decrease-width "decrease width")
  ("q" nil "quit" :exit t))

(provide 'colemacs-layouts-windows-projects)
