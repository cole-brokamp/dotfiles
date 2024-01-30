(cole/leader-keys
  "O" '(:ignore t :which-key "org")
  "Oa" '(org-agenda :which-key "org agenda")
  "Oc" '(counsel-org-capture :which-key "org capture")
)

(cole/local-leader-keys org-mode-map
  "a" '(org-agenda :which-key "agenda")
  "c" '(counsel-org-capture :which-key "capture")
  "," 'org-ctrl-c-ctrl-c
  "*" 'org-ctrl-c-star
  "-" 'org-ctrl-c-minus
  "RET" 'org-ctrl-c-ret
  "TAB" 'org-ctrl-c-tab
  "i" '(:ignore t :which-key "insert")
  "il" '(org-insert-link :which-key "insert link")
  "it" '(counsel-org-tag :which-key "insert tag")
  "iH" '(org-insert-heading-after-current :which-key "insert new heading")
  "ih" '(org-insert-subheading :which-key "insert subheading")
  "d" '(:ignore t :which-key "date")
  "ds" '(org-schedule :which-key "schedule")
  "dd" '(org-deadline :which-key "deadline")
  "dt" '(org-time-stamp-inactive :which-key "timestamp (inactive)")
  "s" '(:ignore t :which-key "subtree")
  "sa" '(org-archive-subtree :which-key "archive subtree")
  "sr" '(org-refile :which-key "refile")
  "n" '(:ignore t :which-key "narrow")
  "nn" '(org-narrow-to-subtree :which-key "narrow")
  "nw" '(widen :which-key "widen")
  "p" '(cole/present/body :which-key "present") ;; but how to only act on narrowed subtree?
  ) 

(defun cole/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun cole/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

  (setq org-todo-keyword-faces '(("WAITING" . "dodger blue")
                                 ("WISHING" . "yellow3")
                                 ("TODO" . "OrangeRed1")
                                 ("DONE" . "green4")))
)

(use-package org
  :hook (org-mode . cole/org-mode-setup)
  :custom
  (org-ellipsis "▾")
  (org-agenda-files '("~/icloud/notes"))
  (org-default-notes-file '("~/icloud/notes/refile-beorg.org"))
  (org-todo-keywords '("TODO" "WAITING" "WISHING" "|" "DONE"))
  (org-agenda-span 10)
  (org-agenda-start-day "-3d")
  (org-agenda-start-on-weekday nil)
  (org-agenda-custom-commands '(("n" "agenda + unscheduled tasks"
				 ((agenda "") (tags "-SCHEDULED={.+}-DEADLINE={.+}/+TODO|+WAITING|+WISHING"))
				 )))
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-startup-folded t)
  (org-capture-templates '(
			   ("t" "task" entry
			    (file+headline "~/icloud/notes/_todo.org" "tasks")
			    "* TODO %?" :prepend t)
			   ("p" "post-it" entry
			    (file+headline "~/icloud/notes/_todo.org" "post-it")
			    "* %^{description} %u\n %?" :prepend t)
			   ))
  :config
  ;; don't auto save after refile or archive b/c org collapses the buffer automatically on save?
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers) ; save org buffers after refiling
  ;; (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers) ; save org buffers after archiving
  (cole/org-font-setup)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cole/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cole/org-mode-visual-fill))

(provide 'colemacs-org)
