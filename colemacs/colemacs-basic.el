(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(desktop-save-mode 1)
(setq make-backup-files nil)
(setq delete-by-moving-to-trash t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq confirm-kill-emacs 'y-or-n-p)
(global-hl-line-mode 1)
;; (setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)
(setq help-window-select t)
(global-auto-revert-mode 1)
(setq cursor-in-non-selected-windows nil)
(setq warning-minimum-level :error)
(setq window-combination-resize t)
(setq x-stretch-cursor t)
(delete-selection-mode 1)

; keep custom variables in a different file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq default-directory (concat (getenv "HOME") "/"))

;; keep extra files created by emacs in /tmp
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

; make ESC quit prompts too
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)


(provide 'colemacs-basic)
