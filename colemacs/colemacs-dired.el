(use-package dired
  :commands (dired)
  :ensure nil
  :custom
  (dired-listing-switches "-go --all --classify --group-directories-first --dired --human-readable")
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "n" 'counsel-find-file
    "c" 'dired-do-copy
    "i" 'dired-hide-details-mode
    "s" 'dired-sort-toggle-or-edit
    (kbd "SPC") 'dired-mark
    (kbd "RET") 'dired-find-alternate-file
    "l" 'dired-find-alternate-file
    "h" 'dired-single-up-directory)
  ;; use gls instead of ls when on mac to support listing switches
  (when (string= system-type "darwin")
    (setq insert-directory-program "gls"))
  (dired-async-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; TODO SPC doesn't work in dired mode (have to use Ctl-SPC), fix this!

(use-package osx-trash
  :custom (delete-by-moving-to-trash t)
  :config (osx-trash-setup))

(provide 'colemacs-dired)
