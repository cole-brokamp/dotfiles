(use-package doom-modeline
             :init (doom-modeline-mode 1)
             :custom
	     (doom-modeline-enable-word-count t))

(use-package doom-themes
             :init (load-theme 'doom-dracula t))

;; (custom-set-faces '(default ((t (:background nil :foreground nil)))))
;; (setq frame-background-mode 'dark) ;; or 'light depending on your terminal theme
;; (setq-default mode-line-format nil) ;; Disable mode line coloring if it conflicts

(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(set-face-attribute 'default nil :font "Source Code Pro" :height 150)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 150)

;; use xwidget support built in on macOS and emacs 29, but only for certain URLs
(setq browse-url-browser-function 'xwidget-webkit-browse-url)
(setq browse-url-default-scheme "https")
(setq browse-url-handlers
      '(("127.0.0.1.*" . xwidget-webkit-browse-url)
	("." . browse-url-default-browser)))

(provide 'colemacs-gui)
