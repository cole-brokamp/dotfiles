;; performance tweaks
(setq read-process-output-max (* 1024 1024))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((ess-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-clients-r-server-command '("R" "--quiet" "--no-save" "-e" "languageserver::run()"))
  (lsp-idle-delay 0.500)
  (lsp-completion-provider :capf)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))


(use-package ess
  :custom
  (ess-eval-visibly 'nowait)
  (ess-auto-width 'window)
  (ess-auto-width-visible nil)
  (ess-use-tracebug nil)
  (ess-style 'RStudio)
  (ess-indent-with-fancy-comments nil)
  (ess-indent-offset 2)
  (ess-help-own-frame nil)
  (ess-help-reuse-window t)
  (ess-ask-for-ess-directory nil)
  (inferior-R-args "--no-save --quiet")
  (ess-S-quit-kill-buffers-p "t")
  (comint-scroll-to-bottom-on-input nil)
  (comint-scroll-to-bottom-on-output nil)
  (comint-move-point-for-output nil)
  (comint-scroll-show-maximum-output nil)
  (ess-use-flymake nil)
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords   . t)
     (ess-R-fl-keyword:constants  . t)
     (ess-R-fl-keyword:modifiers  . t)
     (ess-R-fl-keyword:fun-defs   . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:%op%       . t)
     (ess-fl-keyword:fun-calls)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)))
  (inferior-ess-r-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t)
     (ess-R-fl-keyword:messages . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:matrix-labels)
     (ess-fl-keyword:fun-calls)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)))
  (
   display-buffer-alist
   `(("*R"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-width . 0.5)
      (dedicated . t)
      (reusable-frames . nil))
     ("*Help"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . 1)
      (window-width . 0.33)
      (reusable-frames . nil))))
  :config
  ;; (define-key ess-mode-map (kbd "C-;") 'ess-switch-to-inferior-or-script-buffer)
  (add-hook 'ess-mode-hook 'prettify-symbols-mode)
  ;; (add-hook 'ess-mode-hook 'lsp-deferred)
  )

(cole/local-leader-keys emacs-lisp-mode-map
  "e" '(eval-defun :which-key "eval defun"))


(cole/local-leader-keys ess-mode-map
  "," '(ess-eval-line-and-step :which-key "eval line and step")
  "e" '(ess-eval-region-or-function-or-paragraph-and-step :which-key "eval R/F/P and step")
  "o" '(cole/ess-eval-word :which-key "print object")
  "O" '(cole/ess-glimpse-word :which-key "glimpse object")
  "g" '(:ignore t :which-key "graphics")
  "gg" '(cole/ess-graphics-view :which-key "open graphics server")
  "gs" '(cole/ess-graphics-start :which-key "start graphics server")
  "gx" '(cole/ess-graphics-stop :which-key "stop graphics server")
  "=" '(format-all-buffer t :which-key "format buffer")
  "d" '(:ignore t :which-key "devtools")
  "dl" '(cole/ess-devtools-load-all :which-key "load_all")
  "dt" '(cole/ess-devtools-test :which-key "test")
  "dd" '(cole/ess-devtools-document :which-key "document")
  "dc" '(cole/ess-devtools-check :which-key "check")
  "dr" '(cole/ess-devtools-build-readme :which-key "build readme from Rmd")
  "ds" '(cole/ess-devtools-build-site :which-key "build pkgdown site")
  "l" '(:ignore t :which-key "language server")
  "ls" '(:ignore t :which-key "session")
  "lr" '(lsp-workspace-restart :which-key "restart")
  ;; "lr" '(lsp-rename :which-key "rename everywhere")
  "l=" '(lsp-format-buffer :which-key "format document")
  "h" '(lsp-ui-doc-glance :which-key "help glance")
  "H" '(lsp-describe-thing-at-point :which-key "help window")
;; lsp-ui-doc-focus-frame
;; lsp-ui-doc-unfocus-frame
  ;; use Ctrl-g to quit doc frame
  "r" '(:ignore t :which-key "renv")
  "rS" '(cole/ess-renv-status :which-key "status")
  "rs" '(cole/ess-renv-snapshot :which-key "snapshot")
  "rr" '(cole/ess-renv-restore :which-key "restore")
  "s" '(:ignore t :which-key "session")
  "si" '(ess-interrupt :which-key "interrupt")
  "sr" '(inferior-ess-reload :which-key "reload")
  "ss" '(ess-switch-process :which-key "switch")
  "sq" '(ess-quit :which-key "quit")
  "t" '(:ignore t :which-key "toggle")
  "th" '(lsp-toggle-symbol-highlight :which-key "symbol highlighting")
  "td" '(lsp-ui-doc-mode :which-key "documentation popups")
  "tf" '(lsp-toggle-on-type-formatting :which-key "on type formatting")
  "tD" '(lsp-modeline-diagnostics-mode :which-key "modeline diagnostics")
  "tS" '(lsp-ui-sideline-mode :which-key "sideline")
  "tb" '(lsp-headerline-breadcrumb-mode :which-key "breadcrumbs")
  "t=" '(format-all-mode :which-key "format on save")
  "c" '(:ignore t :which-key "chunks")
  ;; "w" 'ess-execute-screen-options
  )

;; just add pipe globally b/c I can't get it to work only for ess-mode-map
(global-set-key (kbd "C-'") 'cole/insert-pipe)
(global-set-key (kbd "TAB") 'evil-complete-previous)

(defun cole/ess-devtools-load-all ()
  (interactive)
  (ess-eval-linewise "devtools::load_all()"))

(defun cole/ess-devtools-build-readme ()
  (interactive)
  (ess-eval-linewise "devtools::build_readme()"))

(defun cole/ess-devtools-build-site ()
  (interactive)
  (ess-eval-linewise "pkgdown::build_site()"))

(defun cole/ess-devtools-document ()
  (interactive)
  (ess-eval-linewise "devtools::document()"))

(defun cole/ess-devtools-test ()
  (interactive)
  (ess-eval-linewise "devtools::test()"))

(defun cole/ess-devtools-check ()
  (interactive)
  (ess-eval-linewise "devtools::check()"))

(defun cole/ess-renv-status ()
  (interactive)
  (ess-eval-linewise "renv::status()"))

(defun cole/ess-renv-snapshot ()
  (interactive)
  (ess-eval-linewise "renv::snapshot()"))

(defun cole/ess-renv-restore ()
  (interactive)
  (ess-eval-linewise "renv::restore()"))

(defun cole/ess-graphics-stop ()
  (interactive)
  (ess-eval-linewise "httpgd::hgd_close()"))

(defun cole/ess-graphics-start ()
  (interactive)
  (ess-eval-linewise "httpgd::hgd()"))

(defun cole/ess-graphics-view ()
  (interactive)
  (ess-eval-linewise "httpgd::hgd_browse()"))

(defun cole/insert-pipe ()
  "Insert a |>"
  (interactive)
  (just-one-space 1)
  (insert "|>")
  (reindent-then-newline-and-indent))

(defun cole/ess-edit-word-at-point ()
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

(defun cole/ess-eval-word ()
  (interactive)
  (let ((x (cole/ess-edit-word-at-point)))
    (ess-eval-linewise (concat x))))

(defun cole/ess-glimpse-word ()
  (interactive)
  (let ((x (cole/ess-edit-word-at-point)))
    (ess-eval-linewise (concat "pillar::glimpse(" x ")"))))

;; polymode --------------------------------------------------------------------
;; (use-package polymode
;;   :init
;;   (require 'poly-R)
;;   (require 'poly-markdown)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;;   (add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
;;   (add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
;;   (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
;;   )

(use-package poly-R)

;; (defun R-docker ()
;;   (interactive)
;;   (let ((ess-r-customize-alist
;;           (append ess-r-customize-alist
;;                   '((inferior-ess-program . "/home/francois/start-r-docker.sh"))))
;;         (ess-R-readline t))
;;     (R)))

(provide 'colemacs-rstats)
