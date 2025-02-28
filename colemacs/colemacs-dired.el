(cole/leader-keys
  "d" '(dired-jump :which-key "dired")
  "D" '(project-dired :which-key "dired in project root")
  )

(use-package dired
  :commands (dired)
  :ensure nil
  :custom
  (dired-listing-switches "-go --all --classify --group-directories-first --dired --human-readable")
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  ;; (dired-omit-files "^\\..*$\\|^\\.\\.$")
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . all-the-icons-dired-mode)
  ;; (dired-mode . dired-omit-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "n" 'counsel-find-file
    "c" 'dired-do-copy
    "i" 'dired-hide-details-mode
    "s" 'dired-sort-toggle-or-edit
    "I" 'dired-toggle-read-only
    ;; (kbd "SPC") 'dired-mark
    (kbd "RET") 'dired-find-alternate-file
    "l" 'dired-find-alternate-file
    "h" 'dired-up-directory)
  ;; use gls instead of ls when on mac to support listing switches
  (when (string= system-type "darwin")
    (setq insert-directory-program "gls")))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;; use TAB to toggle open subtrees in dired
(use-package dired-subtree
  :custom
  (dired-subtree-line-prefix "    ")
  (dired-subtree-use-backgrounds nil))


(use-package all-the-icons-dired)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Flaten display of nested directories with no other content.
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package osx-trash
  :custom (delete-by-moving-to-trash t)
  :config (osx-trash-setup))

(put 'dired-find-alternate-file 'disabled nil)

(provide 'colemacs-dired)
