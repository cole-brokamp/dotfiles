                             ;; User Info
(setq user-full-name "Cole Brokamp")
(setq user-mail-address "cole.brokamp@gmail.com")

;; use-package
;; Install use-package if necessary
(eval-when-compile
  (require 'package)
  (setq package-enable-at-startup nil)
    (setq package-archives '(("melpa"     . "http://melpa.org/packages/")
                             ("elpa"      . "http://elpa.gnu.org/packages/")
                             ("repo-org"  . "http://orgmode.org/elpa/")))
    (package-initialize)
    (unless package--initialized (package-initialize t))

    ;; Bootstrap `use-package'
    (unless (package-installed-p 'use-package) ; unless it is already installed
      (package-refresh-contents) ; update packages archive
      (package-install 'use-package)) ; and install the most recent version of use-package

    (require 'use-package)
    ; (require 'diminish)
    ; (require 'bind-key)
    (setq use-package-always-ensure t)
    )

(use-package spacemacs-theme
             :defer t
             :init (load-theme 'spacemacs-dark t)
             )

(use-package which-key
             :config (which-key-mode 1)
             )

(use-package general
             :after which-key
             :config
             (general-override-mode 1)

             (defun find-user-init-file ()
               "Edit the `user-init-file', in same window."
               (interactive)
               (find-file user-init-file))
             (defun load-user-init-file ()
               "Load the `user-init-file', in same window."
               (interactive)
               (load-file user-init-file))

             (general-create-definer tyrant-def
                                     :states '(normal visual insert motion emacs)
                                     :prefix "SPC"
                                     :non-normal-prefix "C-SPC")

             (general-create-definer despot-def
                                     :states '(normal insert)
                                     :prefix "SPC"
                                     :non-normal-prefix "C-SPC")

             (general-define-key
               :keymaps 'key-translation-map
               "ESC" (kbd "C-g"))

             (tyrant-def

               ""     nil
               "c"   (general-simulate-key "C-c")
               "h"   (general-simulate-key "C-h")
               "u"   (general-simulate-key "C-u")
               "x"   (general-simulate-key "C-x")
               "SPC" (general-simulate-key "M-x")

               ;; Package manager
               "lp"  'list-packages

               ;; ;; Theme operations
               ;; "t"   '(:ignore t :which-key "themes")
               ;; "tn"  'my/cycle-theme
               ;; "tt"  'load-theme
               ;; "tl"  'load-leuven-theme
               ;; "td"  'load-dichromacy-theme

               ;; Quit operations
               "q"   '(:ignore t :which-key "quit emacs")
               "qq"  'kill-emacs
               "qz"  'delete-frame

               ;; Buffer operations
               "b"   '(:ignore t :which-key "buffer")
               "bb"  'mode-line-other-buffer
               "bd"  'kill-this-buffer
               "b]"  'next-buffer
               "b["  'previous-buffer
               "bq"  'kill-buffer-and-window
               "bR"  'rename-file-and-buffer
               "br"  'revert-buffer

               ;; Window operations
               "w"   '(:ignore t :which-key "window")
               "wm"  'maximize-window
               "w/"  'split-window-horizontally
               "wv"  'split-window-vertically
               "wm"  'maximize-window
               "wu"  'winner-undo
               "ww"  'other-window
               "wd"  'delete-window
               "wD"  'delete-other-windows

               ;; File operations
               "f"   '(:ignore t :which-key "files")
               "fc"  'write-file
               "fe"  '(:ignore t :which-key "emacs")
               "fed" 'find-user-init-file
               "feR" 'load-user-init-file
               "fj"  'dired-jump
               "fl"  'find-file-literally
               "fR"  'rename-file-and-buffer
               "fs"  'save-buffer

               ;; Applications
               "a"   '(:ignore t :which-key "Applications")
               "ad"  'dired
               ":"   'shell-command
               ";"   'eval-expression
               "ac"  'calendar
               "oa"  'org-agenda)

             (general-def 'normal doc-view-mode-map
                          "j"   'doc-view-next-line-or-next-page
                          "k"   'doc-view-previous-line-or-previous-page
                          "gg"  'doc-view-first-page
                          "G"   'doc-view-last-page
                          "C-d" 'doc-view-scroll-up-or-next-page
                          "C-f" 'doc-view-scroll-up-or-next-page
                          "C-b" 'doc-view-scroll-down-or-previous-page)

             (general-def '(normal visual) outline-minor-mode-map
                          "zn"  'outline-next-visible-heading
                          "zp"  'outline-previous-visible-heading
                          "zf"  'outline-forward-same-level
                          "zB"  'outline-backward-same-level)

             (general-def 'normal package-menu-mode-map
                          "i"   'package-menu-mark-install
                          "U"   'package-menu-mark-upgrades
                          "d"   'package-menu-mark-delete
                          "u"   'package-menu-mark-unmark
                          "x"   'package-menu-execute
                          "q"   'quit-window)

             (general-def 'normal calendar-mode-map
                          "h"   'calendar-backward-day
                          "j"   'calendar-forward-week
                          "k"   'calendar-backward-week
                          "l"   'calendar-forward-day
                          "0"   'calendar-beginning-of-week
                          "^"   'calendar-beginning-of-week
                          "$"   'calendar-end-of-week
                          "["   'calendar-backward-year
                          "]"   'calendar-forward-year
                          "("   'calendar-beginning-of-month
                          ")"   'calendar-end-of-month
                          "SPC" 'scroll-other-window
                          "S-SPC" 'scroll-other-window-down
                          "<delete>" 'scroll-other-window-down
                          "<"   'calendar-scroll-right
                          ">"   'calendar-scroll-left
                          "C-b" 'calendar-scroll-right-three-months
                          "C-f" 'calendar-scroll-left-three-months
                          "{"   'calendar-backward-month
                          "}"   'calendar-forward-month
                          "C-k" 'calendar-backward-month
                          "C-j" 'calendar-forward-month
                          "gk"  'calendar-backward-month
                          "gj"  'calendar-forward-month
                          "v"   'calendar-set-mark
                          "."   'calendar-goto-today
                          "q"   'calendar-exit)
             )

(use-package suggest
             :general (tyrant-def "as" 'suggest))

(use-package evil
             :hook (after-init . evil-mode)
             :config
             (evil-set-initial-state 'shell-mode 'normal)
             (evil-set-initial-state 'doc-view-mode 'normal)
             (evil-set-initial-state 'package-menu-mode 'normal)
             (evil-set-initial-state 'biblio-selection-mode 'motion)
             (setq doc-view-continuous t)
             :general
             (tyrant-def
               "wh"  'evil-window-left
               "wl"  'evil-window-right
               "wj"  'evil-window-down
               "wk"  'evil-window-up
               "bN"  'evil-buffer-new
               "fd"  'evil-save-and-close)
             ('motion override-global-map
              "]b"  'evil-next-buffer
              "[b"  'evil-prev-buffer))

(use-package evil-escape
  :after evil
  :config
  (setq-default evil-escape-key-sequence "jk")
  )

(use-package evil-ediff
  :after ediff
  )

(use-package helm-osx-app
  :general
  (tyrant-def
    "ao" 'helm-osx-app)
  )

(use-package osx-trash
  :init (osx-trash-setup)
  )

(use-package reveal-in-osx-finder
  :general
  (tyrant-def
    "bf" 'reveal-in-osx-finder)
    )

;; Splash Screen to Org-mode
;; (setq 
;;   inhibit-splash-screen t
;;   initial-scratch-message nil
;;   initial-major-mode 'markdown-mode
;;  )

;; Beacon mode
;; (use-package beacon
;;              :ensure t
;;              :config
;;              (beacon-mode 1)
;;              )

;; icons
;; (use-package all-the-icons
;;              :ensure t
;;              )

;; Set default fill column
;; (setq-default fill-column 80)

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Disable menu bars, etc.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; No Backup Files
(setq make-backup-files nil)

; open emacs full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; shorter yes or no prompt
(fset 'yes-or-no-p 'y-or-n-p)

;; font
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-15"))
(set-cursor-color "#FF69B4")

;;; Customize the modeline
;; (setq line-number-mode 1)
;; (setq column-number-mode 1)
;; (setq ns-use-srgb-colorspace nil)
;; (use-package spaceline-config
;;              :ensure spaceline
;;              :config
;;              ;; Set some parameters of the spaceline
;;              (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;              )

;; (use-package powerline
;;              :ensure t
;;              :after spaceline-config
;;              :config
;;              (setq
;;                powerline-height (truncate (* 1.0 (frame-char-height)))
;;                powerline-default-separator 'utf-8
;;                powerline-color1 "gray30"
;;                powerline-color2 "gray45"
;;                )
;;              )


;; (use-package rainbow-delimiters
;;              :ensure t
;;              :defer t
;;              :init
;;              (dolist (hook '(text-mode-hook prog-mode-hook emacs-lisp-mode-hook))
;;                (add-hook hook #'rainbow-delimiters-mode))
;;              )

;; (use-package tramp
;;              :ensure t
;;              :defer t
;;              )

;; (use-package neotree
;;              :ensure t
;;              :defer t
;;              :config
;;              (global-set-key [f8] 'neotree-toggle)
;;              )

;; == Markdown ==
;; (use-package markdown-mode
;;              :ensure t
;;              :defer t
;;              :mode (("\\.text\\'" . markdown-mode)
;;                     ("\\.markdown\\'" . markdown-mode)
;;                     ("\\.md\\'" . markdown-mode))
;;              )

;; (use-package flyspell
;;              :defer t
;;              )

;; == LaTex / AucTeX ==
;; (use-package tex
;;              :defer t
;;              :ensure auctex
;;              :config
;;              (setq TeX-auto-save t)
;;              (setq TeX-parse-self t)
;;              (setq-default TeX-master nil)
;;              (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;              (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;              (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;              (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;              (setq reftex-plug-into-AUCTeX t)
;;              (setq TeX-PDF-mode t)
;;              (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
;;              )

;; == Projectile ==
;; (use-package projectile
;;              :ensure t
;;              :defer t
;;              ;;             :diminish projectile-mode
;;              :init
;;              (projectile-mode)
;;              )

;; (use-package magit
;;              :ensure t
;;              :defer t
;;              )

;; (use-package aggressive-indent
;;              :ensure t
;;              :defer t
;;              :init
;;              (global-aggressive-indent-mode 1)
;;              (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode)
;;              )

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Color keywords
;; (add-hook 'prog-common-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\|\\WARNING):" 1 font-lock-warning-face t)))))

;; yaml

;; (use-package yaml-mode
;;              :ensure t
;;              :defer t
;;              :init
;;              (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;              ;; remove auto-fill for yaml mode
;;              (add-hook 'yaml-mode-hook
;;                        '(lambda ()
;;                           (auto-fill-mode -1)
;;                           ))
;;              )

;; polymode --------------------------------------------------------------------
;; (use-package polymode
;;              :ensure t
;;              :init
;;              (require 'poly-R)
;;              (require 'poly-markdown)
;;              :config
;;              (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;;              (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;;              (add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
;;              (add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
;;              (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
;;              )

;; ESS -------------------------------------------------------------------------
;; (use-package ess
;;              :ensure t
;;              :commands R
;;              :config
;;              (setq ess-eval-visibly t)
;; 
;;              ;; R process in its own buffer
;;              (setq inferior-ess-same-window nil)
;; 
;;              ;; ESS style
;;              (setq ess-default-style 'RStudio)
;; 
;;              ;; All help buffers are shown in one dedicated frame
;;              (setq ess-help-own-frame 'one)
;; 
;;              ;; Rd mode
;;              (add-to-list 'auto-mode-alist '("\\.rd\\'" . Rd-mode))
;;              (add-hook 'Rd-mode-hook
;;                        (lambda ()
;;                          (abbrev-mode 1)
;;                          (font-lock-mode 1)))
;;              ;; Cursor always at the end of eval (from ESS-help 20110911)
;;              (setq comint-scroll-to-bottom-on-input t)
;;              (setq comint-scroll-to-bottom-on-output t)
;;              (setq comint-move-point-for-output t)
;;              (setq comint-scroll-show-maximum-output t)
;; 
;;              ;; redefine previous/commands
;;              (define-key comint-mode-map [(meta ?p)] 'comint-previous-matching-input-from-input)
;;              (define-key comint-mode-map [(meta ?n)] 'comint-next-matching-input-from-input)
;;              )

;; (defun R-docker ()
;;   (interactive)
;;   (let ((ess-r-customize-alist
;;           (append ess-r-customize-alist
;;                   '((inferior-ess-program . "/home/francois/start-r-docker.sh"))))
;;         (ess-R-readline t))
;;     (R)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (reveal-in-osx-finder osx-clipboard-mode helm-osx-app evil-escape yaml-mode which-key use-package suggest spacemacs-theme spaceline rainbow-delimiters projectile neotree markdown-mode magit general evil ess beacon all-the-icons aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
