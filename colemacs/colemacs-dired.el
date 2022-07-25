(use-package dired
  :commands (dired)
  :custom
  (dired-listing-switches "-go --all --classify --group-directories-first --dired --human-readable")
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "c" 'counsel-find-file
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  ;; use gls instead of ls when on mac to support listing switches
  (when (string= system-type "darwin")
    (setq insert-directory-program "gls")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; TODO SPC doesn't work in dired mode (have to use Ctl-SPC), fix this!

(use-package dired-single
  :custom
(dired-single-use-magic-buffer t))

(use-package osx-trash
  :custom (delete-by-moving-to-trash t)
  :config (osx-trash-setup))

(use-package all-the-icons)
;; don't forget to run all-the-icons-install-fonts

;; TODO use macOS emoji font for emoticons
;; (when (fboundp 'set-fontset-font)
;;   (set-fontset-font "fontset-default"
;;                     '(#x1F600 . #x1F64F)
;;                     (font-spec :name "Apple Color Emoji") nil 'prepend))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'colemacs-dired)
