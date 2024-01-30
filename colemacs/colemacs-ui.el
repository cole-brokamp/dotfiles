(cole/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tL" '(visual-line-mode :which-key "visual line mode")
  "tf" '(toggle-frame-fullscreen :which-key "full screen")
  "ti" '(highlight-indent-guides-mode :which-key "indent highlights")
  "tl" '(toggle-truncate-lines :which-key "truncate lines")
  "tm" '(toggle-frame-maximized :which-key "maximize screen")
  "tn" '(display-line-numbers-mode :which-key "numbers for lines")
  "tt" '(counsel-load-theme :which-key "choose theme")
  )

(use-package all-the-icons)
;; don't forget to run all-the-icons-install-fonts

(use-package all-the-icons-ivy-rich)
(use-package all-the-icons-ibuffer)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  )

(use-package doom-modeline
             :init (doom-modeline-mode 1)
             :custom
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

(use-package origami
             :init (global-origami-mode))

(use-package hl-todo
  :init (global-hl-todo-mode)
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t)))))

(use-package hydra)

;; use xwidget support built in on macOS and emacs 29, but only for certain URLs
(setq browse-url-browser-function 'xwidget-webkit-browse-url)
(setq browse-url-default-scheme "https")
(setq browse-url-handlers
      '(("127.0.0.1.*" . xwidget-webkit-browse-url)
	("." . browse-url-default-browser)))


(provide 'colemacs-ui)
