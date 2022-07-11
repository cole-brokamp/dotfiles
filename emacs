; -*- mode: emacs-lisp; -*-

(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-percentage 0.6)

(add-to-list 'load-path "~/dotfiles/colemacs")

(require 'colemacs-basic)
(require 'colemacs-package)
(require 'colemacs-ui)
(require 'colemacs-functions)
(require 'colemacs-general)
(require 'colemacs-ivy)
(require 'colemacs-evil)

(require 'colemacs-layouts)
(require 'colemacs-git)
(require 'colemacs-org)
(require 'colemacs-shell)

(require 'colemacs-applications)
(require 'colemacs-dired)
;; (require 'colemacs-markdown)
;; (require 'colemacs-rstats)
;; (require 'colemacs-latex)
;; (require 'colemacs-lang)

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
(setq gc-cons-threshold (* 5 1000 1000))

; track startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

