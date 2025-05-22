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
  (ess-use-tracebug t)
  (ess-use-flymake nil)
  :config
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
  (add-hook 'ess-r-mode-hook 'my-ess-r-mode-hook)
  ;; (define-key ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  )

(defun my-ess-r-mode-hook ()
  (define-key ess-r-mode-map (kbd "C-'") 'cole/insert-pipe))
  

(defun my-inferior-ess-init ()
  (define-key inferior-ess-r-mode-map (kbd "C-'") 'cole/insert-pipe)
  ;; (setq-local ansi-color-for-comint-mode 'filter)
  )

(cole/local-leader-keys ess-mode-map
  "," '(ess-eval-line-and-step :which-key "eval line and step")
  "=" '(format-all-buffer :which-key "format buffer")
  "b" '(cole/r-ess-bp/body :which-key "breakpoints")
  "c" '(:ignore t :which-key "chunks")
  "d" '(cole/r-devtools/body :which-key "devtools")
  "e" '(ess-eval-paragraph-and-step :which-key "eval R/F/P and step")
  "g" '(cole/r-graphics/body :which-key "graphics")
  "h" '(lsp-describe-thing-at-point :which-key "help in new window")
  "H" '(cole/ess-web-help :which-key "help in browser")
  "o" '(cole/ess-eval-word :which-key "print object")
  "O" '(cole/ess-glimpse-word :which-key "glimpse object")
  "s" '(:ignore t :which-key "session")
  "si" '(ess-interrupt :which-key "interrupt")
  "sq" '(ess-quit :which-key "quit")
  "sr" '(inferior-ess-reload :which-key "reload")
  "ss" '(ess-switch-process :which-key "switch")
  "t" '(cole/r-test/body :which-key "test")
  "w" '(ess-watch-add :which-key "watch")
  )

;; (defhydra cole/r-ess-bp ()
;;   "ess breakpoints"
;;   ("b" ess-bp-set "set")
;;   ("x" ess-bp-kill "kill")
;;   ("X" ess-bp-kill-all "kill all")
;;   ("t" ess-bp-toggle-state "toggle state")
;;   ("Q" (lambda () (interactive) (ess-eval-linewise "Q")) "quit debugger")
;;   ("q" nil "quit" :exit t)
;; )

(defhydra cole/r-devtools (:exit t)
  "devtools package actions"
  ("l" (lambda () (interactive) (ess-eval-linewise "devtools::load_all()")) "load all")
  ("c" (lambda () (interactive) (ess-eval-linewise "devtools::check()")) "check")
  ("d" (lambda () (interactive) (ess-eval-linewise "devtools::document()")) "document")
  ("r" (lambda () (interactive) (ess-eval-linewise "devtools::build_readme()")) "build readme from Rmd")
  ("e" (lambda () (interactive) (ess-eval-linewise "devtools::run_examples()")) "run examples")
  ("s" (lambda () (interactive) (ess-eval-linewise "devtools::build_site()")) "build pkgdown site")
  ("m" (lambda () (interactive) (ess-eval-linewise "devtools::build_manual()")) "build pdf manual")
  ("R" (lambda () (interactive) (ess-eval-linewise "rextendr::document()")) "rextendr document")
)

(defhydra cole/r-test (:exit t)
  "test with devtools"
  ("t" (lambda () (interactive) (ess-eval-linewise "devtools::test()")) "test package")
  ("c" (lambda () (interactive) (ess-eval-linewise "devtools::test_coverage()")) "coverage")
  ("s" (lambda () (interactive) (ess-eval-linewise "testthat::snapshot_review()")) "snapshot review")
  ("f" cole/ess-devtools-test-file "file")
)

(defhydra cole/r-graphics (:exit t)
  "graphics httpgd server"
  ("g" (lambda () (interactive) (ess-eval-linewise "if (!names(dev.cur()) == 'unigd') httpgd::hgd(); httpgd::hgd_browse()")) "open")
  ("s" (lambda () (interactive) (ess-eval-linewise "httpgd::hgd()")) "start")
  ("x" (lambda () (interactive) (ess-eval-linewise "httpgd::hgd_close()")) "close")
  ("q" nil "quit" :exit t)
)
  
; TODO how to get file name of active buffer?
(defun cole/ess-devtools-test-file ()
  (interactive)
  (ess-eval-linewise "testthat::test_file()"))

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

(provide 'colemacs-rstats)
