(setq use-dialog-box nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(global-prettify-symbols-mode 1)

(setq display-time-load-average-threshold 5
      display-time-day-and-date t)
(display-time)

(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; (use-package smooth-scrolling
;;   :config
;;   (smooth-scrolling-mode 1)
;;   :custom
;;   (smooth-scroll-margin 10)
;;   )

(setq scroll-margin 0)
(setq scroll-preserve-screen-position nil)
(setq next-screen-context-lines 2)

;; line numbers
(column-number-mode)
; (global-display-line-numbers-mode t)
(global-visual-line-mode 1)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)

; enable line numbers for some modes
(dolist (mode '(
		ess-mode-hook
		ess-r-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; use xwidget support built in on macOS and emacs 28
;(setq browse-url-browser-function 'xwidget-webkit-browse-url)

;; thresholds for "other" windows
(setq split-height-threshold 80)
(setq split-width-threshold 160)

; fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 130)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 130)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  )

(use-package doom-modeline
             :init (doom-modeline-mode 1)
             :custom
	     (doom-modeline-height 15)
	     (doom-modeline-lsp t)
	     (doom-modeline-enable-word-count t))

(use-package doom-themes
             :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.1))

(use-package origami)

(provide 'colemacs-ui)
