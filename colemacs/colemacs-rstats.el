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
  ;; (ess-ask-for-ess-directory nil)
  (inferior-ess-R-program "R")
  (inferior-R-args "--no-save --quiet")
  ;; (ess-R-readline t)
  (ess-S-quit-kill-buffers-p nil)
  (comint-scroll-show-maximum-output t)
  (comint-move-point-for-output t)
  ;; (comint-scroll-to-bottom-on-input t)
  ;; (comint-scroll-to-bottom-on-output t)
  (ess-use-tracebug nil)
  (ess-use-flymake t)
  :config
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
  (add-hook 'ess-r-mode-hook 'my-ess-r-mode-hook)
  ;; (define-key ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  )

(defun my-ess-r-mode-hook ()
  (define-key ess-r-mode-map (kbd "C-'") 'cole/insert-pipe))
  

(defun my-inferior-ess-init ()
  (define-key inferior-ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  (setq-local ansi-color-for-comint-mode 'filter))

(cole/local-leader-keys ess-mode-map
  "," '(ess-eval-line-and-step :which-key "eval line and step")
  "e" '(ess-eval-paragraph-and-step :which-key "eval R/F/P and step")
  "o" '(cole/ess-eval-word :which-key "print object")
  "O" '(cole/ess-glimpse-word :which-key "glimpse object")
  "g" '(cole/r-graphics/body :which-key "graphics")
  ;; "gg" '(cole/ess-graphics-view :which-key "open graphics server")
  ;; "gs" '(cole/ess-graphics-start :which-key "start graphics server")
  ;; "gx" '(cole/ess-graphics-stop :which-key "stop graphics server")
  "=" '(format-all-buffer t :which-key "format buffer")
  "t" '(:ignore t :which-key "test")
  "tt" '((lambda () (interactive) (ess-eval-linewise "devtools::test()")) :which-key "test")
  "tf" '(cole/ess-devtools-test-file :which-key "test file")
  "tc" '((lambda () (interactive) (ess-eval-linewise "devtools::test_coverage()")) :which-key "test coverage")
  "ts" '((lambda () (interactive) (ess-eval-linewise "devtools::snapshot_review()")) :which-key "snapshot review")
  "d" '(:ignore t :which-key "devtools")
  "dl" '((lambda () (interactive) (ess-eval-linewise "devtools::load_all()")) :which-key "load all")
  "dl" '((lambda () (interactive) (ess-eval-linewise "devtools::document()")) :which-key "document")
  "dc" '((lambda () (interactive) (ess-eval-linewise "devtools::check()")) :which-key "check")
  "dr" '((lambda () (interactive) (ess-eval-linewise "devtools::build_readme()")) :which-key "build readme from Rmd")
  "ds" '((lambda () (interactive) (ess-eval-linewise "devtools::build_site()")) :which-key "build pkgdown site")
  "dm" '((lambda () (interactive) (ess-eval-linewise "devtools::build_manual()")) :which-key "build pdf manual")
  "h" '(lsp-ui-doc-glance :which-key "help")
  "H" '(lsp-describe-thing-at-point :which-key "help in new window")
  "w" '(cole/ess-web-help :which-key "help in browser")
  "c" '(:ignore t :which-key "chunks")
  "s" '(:ignore t :which-key "session")
  "si" '(ess-interrupt :which-key "interrupt")
  "sr" '(inferior-ess-reload :which-key "reload")
  "ss" '(ess-switch-process :which-key "switch")
  "sq" '(ess-quit :which-key "quit")
  )

(defhydra cole/r-graphics (:exit t)
  "graphics httpgd server"
  ("g" (lambda () (interactive) (ess-eval-linewise "if (!names(dev.cur()) == 'unigd') httpgd::hgd(); httpgd::hgd_browse()")) "open")
  ("s" (lambda () (interactive) (ess-eval-linewise "httpgd::hgd()")) "start")
  ("x" (lambda () (interactive) (ess-eval-linewise "httpgd::hgd_close()")) "close")
  ("q" nil "quit")
  )
  

;; (defun cole/ess-devtools-load-all ()
;;   (interactive)
;;   (ess-eval-linewise "devtools::load_all()"))

; TODO how to get file name of active buffer?
(defun cole/ess-devtools-test-file ()
  (interactive)
  (ess-eval-linewise "testthat::test_file()"))

(defun cole/ess-graphics-stop ()
  (interactive)
  (ess-eval-linewise "httpgd::hgd_close()"))

(defun cole/ess-graphics-start ()
  (interactive)
  (ess-eval-linewise "httpgd::hgd()"))

(defun cole/ess-graphics-view ()
  (interactive)
  (ess-eval-linewise "if (!names(dev.cur()) == 'httpgd') httpgd::hgd(silent = TRUE); httpgd::hgd_browse()"))

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

(defun cole/ess-web-help ()
  (interactive)
  (let ((x (cole/ess-edit-word-at-point)))
    (ess-eval-linewise (concat "help('" x "', help_type = 'html', try.all.packages = TRUE)"))))

(use-package polymode
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))
  )

(use-package poly-R)


(provide 'colemacs-rstats)
