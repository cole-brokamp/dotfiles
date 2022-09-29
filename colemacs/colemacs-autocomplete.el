(use-package company
  :config
  (global-company-mode)
  :custom
  (company-insertion-on-trigger nil)
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  ;; (company-dabbrev-downcase nil)
  ;; (company-dabbrev-ignore-case t)
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-echo-metadata-frontend
     company-preview-frontend))
  (company-backends
   '(company-capf
     company-files
     company-dabbrev-code))
  :bind (:map company-active-map ("<tab>" . company-complete))
  )

;; (global-set-key (kbd "<tab>") #'company-complete)

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'colemacs-autocomplete)
