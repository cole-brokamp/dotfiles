;;; user interface ;;;
(setq initial-scratch-message "✨ welcome \n")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (set-fringe-mode 10)
(menu-bar-mode -1)
;; (setq initial-major-mode 'R-mode)
(setq confirm-kill-emacs 'y-or-n-p)
(global-hl-line-mode 1)
(setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)
(setq help-window-select t)
(global-auto-revert-mode 1)
;; (setq cursor-in-non-selected-windows nil)
;; (setq warning-minimum-level :error)
;; (setq window-combination-resize t)
(setq x-stretch-cursor t)
;; (delete-selection-mode 1)
(setq ring-bell-function 'ignore)
(global-visual-line-mode 1)

;;; emacs behavior ;;;
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)

; keep custom variables in a different file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; (setq default-directory (concat (getenv "HOME") "/"))

;; keep extra files created by emacs in /tmp
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

; make ESC quit prompts too
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; thresholds for "other" windows
;; (setq split-height-threshold 80)
;; (setq split-width-threshold 160)
;; (setq max-mini-window-height 0.5)

;; (setq use-dialog-box nil)
;; (setq visible-bell nil)
;; (global-prettify-symbols-mode 1)

;; (setq display-time-load-average-threshold 5
;;       display-time-day-and-date t)
;; (display-time)

;; (column-number-mode)

(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq scroll-margin 2)
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
;; (setq pixel-scroll-precision-use-momentum nil)

; fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 150)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 150)
; to show emojis
(when (>= emacs-major-version 27)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))


(provide 'colemacs-basic)
