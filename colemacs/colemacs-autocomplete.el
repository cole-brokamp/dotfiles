(use-package company
  :config
  (global-company-mode)
  :custom
  (company-insertion-on-trigger nil)
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.0)))
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  ;; (company-dabbrev-downcase nil)
  ;; (company-dabbrev-ignore-case t)
  (company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend
     company-preview-frontend))
  (company-backends
   '(company-capf
     company-dabbrev-code
     company-keywords
     company-dabbrev
     company-files
     company-ispell))
  :bind (:map company-active-map ("<tab>" . company-complete))
  )

(global-set-key (kbd "C-p") #'company-complete)

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'colemacs-autocomplete)
