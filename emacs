; -*- mode: emacs-lisp; -*-

(setq gc-cons-threshold (* 80 1000 1000))
;; (setq gc-cons-percentage 0.6)

(add-to-list 'load-path "~/dotfiles/colemacs")

(require 'colemacs-basic)
(require 'colemacs-package)
(require 'colemacs-general)
(require 'colemacs-ui)
(require 'colemacs-evil)
(require 'colemacs-ivy)
(require 'colemacs-autocomplete)

(require 'colemacs-functions)

(require 'colemacs-layouts)
(require 'colemacs-git)
(require 'colemacs-org)
(require 'colemacs-shell)
(require 'colemacs-lisp)

(require 'colemacs-dired)
(require 'colemacs-markdown)
(require 'colemacs-treemacs)
(require 'colemacs-rstats)
(require 'colemacs-spelling)
;; (require 'colemacs-latex)
(require 'colemacs-lang)
(require 'colemacs-applications)
(require 'colemacs-pdf)

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
(setq gc-cons-threshold (* 1 1000 1000))

; track startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
