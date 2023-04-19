(use-package ess
  :custom
  (ess-eval-visibly 'nowait)
  (ess-auto-width 'window)
  ;; (ess-auto-width-visible nil)
  (ess-style 'RStudio)
  (tab-always-indent 'complete)
  (ess-indent-with-fancy-comments nil)
  (ess-help-own-frame nil)
  (ess-help-reuse-window t)
  (ess-ask-for-ess-directory nil)
  (inferior-ess-R-program "R")
  (ess-R-readline t)
  (inferior-R-args "--no-save --quiet")
  (ess-S-quit-kill-buffers-p nil)
  (comint-scroll-show-maximum-output t)
  (comint-move-point-for-output t)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (ess-use-tracebug nil)
  (ess-use-flymake t)
  :config
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
  (define-key ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  )

(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))

(cole/local-leader-keys ess-mode-map
  "," '(ess-eval-line-and-step :which-key "eval line and step")
  "e" '(ess-eval-paragraph-and-step :which-key "eval R/F/P and step")
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
  "h" '(ess-help :which-key "help")
  ;; "H" '(ess-display-help-in-browser :which-key "help in browser")
  "i" '(:ignore t :which-key "insert")
  "ic" '(cole/ess-insert-r-code-chunk :which-key "chunk")
  "r" '(:ignore t :which-key "renv")
  "rS" '(cole/ess-renv-status :which-key "status")
  "rs" '(cole/ess-renv-snapshot :which-key "snapshot")
  "rr" '(cole/ess-renv-restore :which-key "restore")
  "s" '(:ignore t :which-key "session")
  "si" '(ess-interrupt :which-key "interrupt")
  "sr" '(inferior-ess-reload :which-key "reload")
  "ss" '(ess-switch-process :which-key "switch")
  "sq" '(ess-quit :which-key "quit")
  "c" '(:ignore t :which-key "chunks")
  )

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
  (ess-eval-linewise "if (!names(dev.cur()) == 'httpgd') httpgd::hgd(silent = TRUE); httpgd::hgd_browse()"))

(defun cole/ess-insert-r-code-chunk ()
  "Insert an R Markdown code chunk."
  (interactive)
  (insert "```{r}\n")
  (save-excursion
    (insert "\n")
    (insert "```\n")))

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

(use-package polymode
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))
  )


;; (defun R-docker ()
;;   (interactive)
;;   (let ((ess-r-customize-alist
;;           (append ess-r-customize-alist
;;                   '((inferior-ess-program . "/home/francois/start-r-docker.sh"))))
;;         (ess-R-readline t))
;;     (R)))


(provide 'colemacs-rstats)
