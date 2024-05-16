(cole/leader-keys
  "C" '(:ignore t :which-key "checking code")
  "Cc" '(flymake-mode :which-key "checking code")
  )

(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode)))

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package rust-mode)

(use-package doc-view
  :custom
  (doc-view-resolution '300)
)

(cole/local-leader-keys doc-view-mode-map
  ;; "q" '((lambda () (interactive) (async-shell-command '(quarto preview))) :which-key "quarto preview")
  "W" '(doc-view-fit-width-to-window :which-key "fit width to window")
  "H" '(doc-view-fit-height-to-window :which-key "fit height to window")
  "f" '(doc-view-presentation-mode :which-key "full screen")
  "F" '(doc-view-presentation-exit :which-key "exit full screen")
  "j" '(doc-view-next-line-or-next-page :which-key "scroll down")
  "k" '(doc-view-previous-line-or-previous-page :which-key "scroll down")
  )

(provide 'colemacs-lang)
