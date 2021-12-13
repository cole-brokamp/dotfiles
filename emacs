; -*- mode: emacs-lisp; -*-

;; basic ==========================================================

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(desktop-save-mode 1)
(setq make-backup-files nil)
(setq delete-by-moving-to-trash t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq confirm-kill-emacs 'y-or-n-p)
(global-hl-line-mode 1)
;; (setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)
(setq help-window-select t)
(global-auto-revert-mode 1)
(setq cursor-in-non-selected-windows nil)
(setq warning-minimum-level :error)
(setq window-combination-resize t)
(setq x-stretch-cursor t)
(delete-selection-mode 1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(global-prettify-symbols-mode 1)

(setq display-time-load-average-threshold 5
      display-time-day-and-date t)
(display-time)

(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)

;; use xwidget support built in on macOS and emacs 28
;(setq browse-url-browser-function 'xwidget-webkit-browse-url)


;; line numbers
(column-number-mode)
; (global-display-line-numbers-mode t)
(global-visual-line-mode 1)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)

; enable line numbers for some modes
(dolist (mode '(
		ess-mode-hook
		ess-r-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq default-directory (concat (getenv "HOME") "/"))

; fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 130)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 130)

;; keep extra files created by emacs in /tmp
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; thresholds for "other" windows
(setq split-height-threshold 60)
(setq split-width-threshold 100)

;; package manager ======================================================

; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; update packages automatically on startup if they haven't been updated in the past 7 days
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  )

;; my functions =======================================================
(defun cole/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun cole/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*Messages*")))

(defun cole/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*scratch*")))

;; packages ================================================================

;; key bindings ===============================================

; make ESC quit prompts too
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

(defun cole/switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if compilation-last-buffer
      (pop-to-buffer compilation-last-buffer)
    (user-error "There is no compilation buffer?")))

(defun cole/show-hide-compilation-window ()
  "Show/Hide the window containing the compilation buffer."
  (interactive)
  (when-let ((buffer compilation-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (cole/switch-to-compilation-buffer))))

(defun cole/buffer-file-name ()
  "Copy current buffer's file name to clipboard, and display it."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(defun cole/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun cole/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun cole/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))
  
(use-package general
  :config
  (general-create-definer cole/leader-keys
    :states '(normal insert visual)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer cole/local-leader-keys
    :states '(normal insert visual)
    :prefix ","
    :non-normal-prefix "C-,")
  )

(cole/local-leader-keys org-mode-map
  "a" '(org-agenda :which-key "agenda")
  "c" '(counsel-org-capture :which-key "capture")
  "," 'org-ctrl-c-ctrl-c
  "*" 'org-ctrl-c-star
  "-" 'org-ctrl-c-minus
  "RET" 'org-ctrl-c-ret
  "TAB" 'org-ctrl-c-tab
  "i" '(:ignore t :which-key "insert")
  "il" '(org-insert-link :which-key "insert link")
  "it" '(counsel-org-tag :which-key "insert tag")
  "iH" '(org-insert-heading-after-current :which-key "insert new heading")
  "ih" '(org-insert-subheading :which-key "insert subheading")
  "d" '(:ignore t :which-key "date")
  "ds" '(org-schedule :which-key "schedule")
  "dd" '(org-deadline :which-key "deadline")
  "dt" '(org-time-stamp-inactive :which-key "timestamp (inactive)")
  "s" '(:ignore t :which-key "subtree")
  "sa" '(org-archive-subtree :which-key "archive subtree")
  "sr" '(org-refile :which-key "refile")
  "n" '(:ignore t :which-key "narrow")
  "nn" '(org-narrow-to-subtree :which-key "narrow")
  "nw" '(widen :which-key "widen")
  "p" '(cole/present/body :which-key "present") ;; but how to only act on narrowed subtree?
  ) 


(cole/leader-keys
  "SPC" '(counsel-M-x :which-key "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "/" 'swiper-all
  "'" '(vterm-toggle-cd :which-key "shell")
  "\"" '(vterm-toggle-show :which-key "new shell")
  ";" '(evilnc-comment-or-uncomment-lines :which-key "comment operator")
  "a" '(:ignore t :which-key "applications")
  ;; "ad" docker
  "ac" 'calendar
  "as" '(vterm :which-key "new vterm shell")
  "ab" '(ivy-bibtex-with-local-bibliography :which-key "bibtex (local bib)") ; auto uses bib file from \bibliography in files!
  "aB" '(ivy-bibtex :which-key "bibtex (global bib)")
  "o" '(:ignore t :which-key "org")
  "oc" '(counsel-org-capture "org capture")
  "oa" '(org-agenda "org agenda")
  ":" '(shell-command :which-key "shell command")
  "q" '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-emacs :which-key "quit")
  "qr" '(restart-emacs :which-key "restart")
  "qf" '(delete-frame :which-key "kill frame")
  "f" '(:ignore t :which-key "files")
  "fs" '(save-buffer :which-key "save")
  "fS" '(evil-write-all :which-key "save all")
  "ff" '(counsel-find-file :which-key "find file")
  "fD" '(delete-file :which-key "delete file")
  "b" '(:ignore t :which-key "buffers")
  "bb" '(ivy-switch-buffer :which-key "switch to buffer")
  "bB" '(counsel-switch-buffer :which-key "switch buffer with preview")
  "bi" '(ibuffer :which-key "ibuffer")
  "bI" '(projectile-ibuffer :which-key "ibuffer in project")
  "bf" '(reveal-in-osx-finder :which-key "show buffer in finder")
  "bd" '(kill-current-buffer :which-key "delete buffer")
  "bm" '(cole/switch-to-messages-buffer :which-key "messages buffer")
  "br" '(revert-buffer :which-key "reload from disk")
  "bR" '(font-lock-update :which-key "reload font locks")
  "bM" '(buf-move :which-key "move buffer")
  "bs" '(cole/switch-to-scratch-buffer :which-key "scratch buffer")
  "bn" '(cole/buffer-file-name :which-key "copy buffer filename")
  "c" '(:ignore t :which-key "compile")
  "cc" '(projectile-compile-project :which-key "compile")
  "ck" '(kill-compilation :which-key "kill compilation")
  "cd" '(cole/show-hide-compilation-window :which-key "show/hide compilation window")
  "i" '(:ignore t :which-key "insert")
  "ie" '(emojify-insert-emoji :which-key "insert emoji")
  "io" '(newline-and-indent :which-key "open line")
  "ij" '(evil-collection-unimpaired-insert-newline-below :which-key "insert line below")
  "ik" '(evil-collection-unimpaired-insert-newline-above :which-key "insert line above")
  "h" '(:ignore t :which-key "help")
  "hf" 'describe-function
  "hh" 'help-for-help
  "hv" 'describe-variable
  "hk" 'describe-key
  "hm" 'describe-mode
  "hb" '(general-describe-keybindings :which-key "key bindings")
  "hc" 'describe-command
  "hp" 'describe-package
  "h." 'display-local-help
  "hr" '(repeat-complex-command :which-key "repeat complex command")
  "j" '(:ignore t :which-key "jump")
  "jd" '(dired-jump :which-key "dired-jump")
  "n" '(:ignore t :which-key "notes")
  "nn" '((lambda () (interactive) (dired "~/icloud/notes")) :which-key "notes")
  "nt" '((lambda () (interactive) (find-file "~/icloud/notes/_todo.org")) :which-key "_todo.org")
  "nw" '((lambda () (interactive) (find-file "~/icloud/notes/wiki.org")) :which-key "wiki.org")
  "ns" '((lambda () (interactive) (find-file "~/icloud/notes/students.org")) :which-key "students.org")
  "nr" '((lambda () (interactive) (find-file "~/icloud/notes/refile-beorg.org")) :which-key "refile-beorg.org")
  "nb" '((lambda () (interactive) (find-file "~/icloud/its_lit_fam/bib.bib")) :which-key "bib.bib")
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "tf" '(toggle-frame-fullscreen :which-key "full screen")
  "tl" '(toggle-truncate-lines :which-key "truncate lines")
  "tL" '(visual-line-mode :which-key "visual line mode")
  "tn" '(display-line-numbers-mode :which-key "numbers for lines")
  "tm" '(toggle-frame-maximized :which-key "maximize screen")
  "w" '(:ignore t :which-key "windows")
  "wd" '(evil-window-delete :which-key "delete window")
  "w/" '(cole/split-window-right-and-focus :which-key "split right")
  "w-" '(cole/split-window-below-and-focus :which-key "split down")
  "wh" '(evil-window-left :which-key "focus left")
  "wl" '(evil-window-right :which-key "focus right")
  "wj" '(evil-window-down :which-key "focus down")
  "wk" '(evil-window-up :which-key "focus up")
  "wH" '(evil-window-move-far-left :which-key "move left")
  "wK" '(evil-window-move-very-top :which-key "move up")
  "wL" '(evil-window-move-far-right :which-key "move right")
  "wJ" '(evil-window-move-very-bottom :which-key "move down")
  "wf" '(make-frame :which-key "make into frame")
  "wm" '(cole/toggle-maximize-buffer :which-key "maximize window")
  "w=" '(balance-windows-area :which-key "equal window areas")
  "e" '(:ignore t :which-key "emacs")
  "ed" '(cole/find-user-init-file :which-key "open emacs dotfile")
  "s" '(:ignore t :which-key "search")
  "ss" '(swiper :which-key "swiper")
  "sr" '(query-replace :which-key "search and replace")
  "sR" '(query-replace-regexp :which-key "search and replace (regex)")
  "r" '(:ignore t :which-key "register")
  "rd" '(view-register :which-key "view")
  "ri" '(insert-register :which-key "insert text")
  "ry" '(copy-to-register :which-key "yank text")
  "rw" '(window-configuration-to-register :which-key "windows save")
  "rf" '(frameset-to-register :which-key "frames save")
  "rj" '(jump-to-register :which-key "jump to")
  "rv" '(view-register :which-key "view")
  "z" '(:ignore t :which-key "fold")
  "zz" '(evil-toggle-fold :which-key "toggle fold")
  "zo" '(evil-open-fold-rec :which-key "open fold")
  "zO" '(evil-open-folds :which-key "open all folds")
  "zc" '(evil-close-fold :which-key "close fold")
  "zC" '(evil-close-folds :which-key "close all folds")
  )

(use-package format-all)

(use-package reveal-in-osx-finder)

(use-package buffer-move)

(use-package restart-emacs)

(use-package ivy
  :diminish
  :bind (
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 ("C-o" . ivy-dispatching-done) ;; C-o for more options in ivy minibuffers
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package osx-trash
  :custom (delete-by-moving-to-trash t)
  :config (osx-trash-setup))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package doom-modeline
             :init (doom-modeline-mode 1)
             :custom
	     (doom-modeline-height 15)
	     (doom-modeline-lsp t)
	     (doom-modeline-enable-word-count t))

(use-package doom-themes
             :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil) ; make TAB work in terminal again
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion (kbd "C-j") 'evil-scroll-line-down)
  (evil-global-set-key 'motion (kbd "C-k") 'evil-scroll-line-up)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  :custom
  (evil-undo-system 'undo-fu))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package undo-fu
  :after evil
  :custom
  (undo-fu-ignore-keyboard-quit 1))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  :custom
  (smooth-scroll-margin 12)
  )

(setq scroll-margin 12) ; to match smooth scrolling margin
(setq scroll-preserve-screen-position 't)
(setq next-screen-context-lines 12)

;; ensure full $PATH makes it into emacs
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-go --all --classify --group-directories-first --dired --human-readable")
  (dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  ;; use gls instead of ls when on mac to support listing switches
  (when (string= system-type "darwin")
    (setq insert-directory-program "gls")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; TODO SPC doesn't work in dired mode (have to use Ctl-SPC), fix this!

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (when (file-directory-p "~/icloud/works")
      (setq projectile-project-search-path '("~/icloud/works")))
  (setq projectile-switch-project-action #'projectile-dired)
  (cole/leader-keys
    "p" '(:ignore t :which-key "projects")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pd" '(projectile-kill-buffers :which-key "close project buffers")
    "ps" '(projectile-ag :which-key "search in project files")
    "pl" '(:ignore t :which-key "TODO - open project in new layout")
  ))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  :custom
  (eyebrowse-new-workspace t)
  :init
  (cole/leader-keys
    "l" '(:ignore t :which-key "layouts")
    "l TAB" '(eyebrowse-last-window-config :which-key "last layout")
    "lc" '(eyebrowse-create-named-window-config :which-key "create layout")
    "lx" '(eyebrowse-close-window-config :which-key "close layout")
    "lr" '(eyebrowse-rename-window-config :which-key "rename layout")
    "ll" '(eyebrowse-switch-to-window-config :which-key "switch to layout")
    "l C-h" '(eyebrowse-prev-window-config :which-key "previous layout")
    "l C-l" '(eyebrowse-next-window-config :which-key "next layout")
    ))

(use-package magit
  :custom
  ((vc-follow-symlinks t))
  :config
  (cole/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gc" '(magit-clone :which-key "git clone")
    "gf" '(magit-find-file :which-key "find file")
  ))

(use-package evil-magit
  :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package git-timemachine
  :config
  (cole/leader-keys
    "gt" '(cole/git-timemachine/body :which-key "time machine")
    ))

(use-package github-browse-file
  :config
  (cole/leader-keys
    "gh" '(github-browse-file :which-key "browse on github")
    ))

;; Org Mode Configuration ------------------------------------------------------

(defun cole/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun cole/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

  (setq org-todo-keyword-faces '(("WAITING" . "dodger blue")
                                 ("WISHING" . "yellow3")
                                 ("TODO" . "OrangeRed1")
                                 ("DONE" . "green4")))
)

(use-package org
  :hook (org-mode . cole/org-mode-setup)
  :custom
  (org-ellipsis "▾")
  (org-agenda-files '("~/icloud/notes"))
  (org-default-notes-file '("~/icloud/notes/refile-beorg.org"))
  (org-todo-keywords '("TODO" "WAITING" "WISHING" "|" "DONE"))
  (org-agenda-span 10)
  (org-agenda-start-day "-3d")
  (org-agenda-start-on-weekday nil)
  (org-agenda-custom-commands '(("n" "agenda + unscheduled tasks"
				 ((agenda "") (tags "-SCHEDULED={.+}-DEADLINE={.+}/+TODO|+WISHING|+WAITING"))
				 )))
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-startup-folded t)
  (org-capture-templates '(
			   ("t" "todo [_todo.org tasks]" entry
			    (file+headline "~/icloud/notes/_todo.org" "Tasks")
			    "* TODO %?")
			   ))
  :config
  ;; don't auto save after refile or archive b/c org collapses the buffer automatically on save?
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers) ; save org buffers after refiling
  ;; (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers) ; save org buffers after archiving
  (cole/org-font-setup)
  )

;org-latex-pdf-process "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"
;org-enable-github-support t
;org-enable-epub-support t
;org-enable-bootstrap-support t
;org-enable-sticky-header nil
;org-enable-reveal-js-support t


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cole/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cole/org-mode-visual-fill))


;; shell ----------------------------------

(use-package vterm
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-always-compile-module t)
  (vterm-use-vterm-prompt-detection-method t)
  (vterm-copy-exclude-prompt t)
  (vterm-kill-buffer-on-exit t))

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (reusable-frames . visible)
                (window-height . 0.3))))

;(with-eval-after-load 'shell
;  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
;  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

(use-package markdown-mode)
(use-package grip-mode)

(cole/local-leader-keys markdown-mode-map
  "p" '(grip-mode :which-key "preview mode (grip)")
  "P" '(markdown-live-preview-mode :which-key "preview mode (native)")
  "o" '(markdown-follow-thing-at-point :which-key "open thing at point")
  "i" '(:ignore t :which-key "insert")
  "il" '(markdown-insert-link :which-key "link")
  "if" '(markdown-insert-foldable-block :which-key "foldable block")
  "ic" '(markdown-insert-code :which-key "code")
  "it" '(markdown-insert-table :which-key "table")
  "ii" '(markdown-insert-image :which-key "image")
  "t" '(:ignore t :which-key "toggle")
  "tm" '(markdown-toggle-math :which-key "math")
  "tf" '(markdown-toggle-fontify-code-blocks-natively :which-key "fontify code blocks") ;; TODO make this on by default
  "ti" '(markdown-toggle-inline-images :which-key "inline images")
  "tu" '(markdown-toggle-url-hiding :which-key "url hiding")
  "tm" '(markdown-toggle-markup-hiding :which-key "markup hiding")
  "x" '(:ignore t :which-key "text")
  )

(use-package dockerfile-mode)

;; spell checking
(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (cole/leader-keys
    "S" '(:ignore t :which-key "spelling")
    "Ss" '(flyspell-mode :which-key "toggle spelling mode")
    "Sp" '(flyspell-prog-mode :which-key "enable spelling prog mode")
    "Sb" '(flyspell-buffer :which-key "spell check buffer")
    "Sc" '(flyspell-correct-wrapper :which-key "correct word"))
  (add-hook 'flyspell-prog-mode-hook (lambda () (message "Flyspell prog-mode enabled in current buffer"))))

;; lsp ----------------------------------------------------------------------

;; performance tweaks
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

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

(use-package avy
  :config
  (setq avy-background t))

(cole/leader-keys
  "sa" '(avy-goto-char-timer :which-key "avy"))

;; latex things
(use-package ivy-bibtex
  :after ivy
  :custom
  (bibtex-completion-bibliography "~/icloud/its_lit_fam/bib.bib")
  (bibtex-completion-library-path "~/icloud/its_lit_fam/bibtex_pdfs/")
  ;; (bibtex-completion-library-path '("/path1/to/pdfs" "/path2/to/pdfs"))
  ;; (bibtex-completion-notes-path "~/icloud/its_lit_fam/bib.org")
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "_")
  (bibtex-autokey-titlewords 1)
  (bibtex-autokey-titleword-stretch 1)
  (bibtex-autokey-titleword-length 6)
  (bibtex-completion-cite-prompt-for-optional-arguments nil))

(use-package org-ref
  :custom
  (org-ref-default-bibliography '("~/icloud/its_lit_fam/bib.bib"))
  (org-ref-bibliography-notes "~/icloud/its_lit_fam/bib.org")
  (org-ref-pdf-directory "~/icloud/its_lit_fam/bibtex_pdfs/")
  )
  
(cole/local-leader-keys bibtex-mode-map
  "b" '(org-ref-bibtex-hydra/body :which-key "ivy-bibtex")
  "o" '(:ignore t "open")
  "ob" '(org-ref-open-in-browser :which-key "open in browser")
  "op" '(org-ref-open-bibtex-pdf :which-key "open associated pdf")
  "oP" '(org-ref-bibtex-pubmed :which-key "open in pubmed")
  "og" '(org-ref-bibtex-google-scholar :which-key "open in google scholar")
  "A" '(org-ref-assoc-pdf-with-entry :which-key "associate pdf")
  "C" '(bibtex-clean-entry :which-key "clean entry")
  "H" '(org-ref-bibtex-hydra/body :which-key "bibtex hydra")
  "n" '(org-ref-open-bibtex-notes :which-key "open notes")
  "t" '(helm-tag-bibtex-entry :which-key "tag")
  "i" '(:ignore t "insert entry")
  "ii" '(doi-utils-add-entry-from-crossref-query "crossref")
  "ip" '(pubmed-insert-bibtex-from-pmid "PMID")
  "id" '(doi-utils-add-bibtex-entry-from-doi "DOI")
  )

;; rstats ess ------------------------------------------------------------------------

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
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output t)
  (comint-scroll-show-maximum-output t)
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
  (ess-r-package-eval-linewise "renv::restore()"))

(defun cole/ess-graphics-stop ()
  (interactive)
  (ess-r-package-eval-linewise "httpgd::hgd_stop()"))

(defun cole/ess-graphics-start ()
  (interactive)
  (ess-r-package-eval-linewise "httpgd::hgd()"))

(defun cole/ess-graphics-view ()
  (interactive)
  (ess-r-package-eval-linewise "httpgd::hgd_browse()"))


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
(use-package polymode
  :ensure t
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
  )

;; (defun R-docker ()
;;   (interactive)
;;   (let ((ess-r-customize-alist
;;           (append ess-r-customize-alist
;;                   '((inferior-ess-program . "/home/francois/start-r-docker.sh"))))
;;         (ess-R-readline t))
;;     (R)))

(use-package hl-todo
  :custom
  (hl-todo-color-background t)
  :init
  (global-hl-todo-mode 1))

(use-package org-tree-slide
  :custom
  (org-tree-slide-slide-in-effect nil))

(use-package emojify
  :custom
  (emojify-display-style 'image)
  :hook (after-init . global-emojify-mode))

(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode)))

(use-package all-the-icons)
;; don't forget to run all-the-icons-install-fonts

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :custom (all-the-icons-ivy-file-commands '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

;; TODO use macOS emoji font for emoticons
;; (when (fboundp 'set-fontset-font)
;;   (set-fontset-font "fontset-default"
;;                     '(#x1F600 . #x1F64F)
;;                     (font-spec :name "Apple Color Emoji") nil 'prepend))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; hydras ====================================================

(use-package hydra)

(defhydra cole/present ()
  "presentation"
  ("t" org-tree-slide-mode "toggle slide mode")
  ("n" org-tree-slide-move-next-tree "next")
  ("p" org-tree-slide-move-previous-tree "previous")
  ("q" nil "quit" :exit t)) ;; how to add another "org-tree-slide-mode 0" to turn it off when quitting the hydra

(defhydra cole/git-timemachine ()
  "git-timemachine"
  ("j" git-timemachine-show-next-revision "next revision")
  ("k" git-timemachine-show-previous-revision "previous revision")
  ("c" git-timemachine-show-commit "show commit")
  ("y" git-timemachine-kill-abbreviated-revision "yank short hash")
  ("q" git-timemachine-quit "quit" :exit t))

(defhydra cole/scale-text (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

;; TODO hydra for resizing windows (SPC w r)
(defhydra cole/resize-window ()
  "resize windows"
  ("j" evil-window-increase-height "increase height")
  ("k" evil-window-decrease-height "decrease height")
  ("l" evil-window-increase-width "increase width")
  ("h" evil-window-decrease-width "decrease width")
  ("q" nil "quit" :exit t))
  

(cole/leader-keys
  "ts" '(cole/scale-text/body :which-key "scale text")
  "wr" '(cole/resize-window/body :which-key "resize window"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(poly-R polymode yaml-mode which-key vterm-toggle visual-fill-column use-package undo-fu smooth-scrolling reveal-in-osx-finder restart-emacs rainbow-delimiters osx-trash org-tree-slide org-ref org-bullets lsp-ui lsp-ivy ivy-rich ivy-prescient ivy-hydra ivy-bibtex hl-todo helpful grip-mode github-browse-file git-timemachine general forge flyspell-correct-ivy eyebrowse exec-path-from-shell evil-surround evil-org evil-nerd-commenter evil-magit evil-escape evil-collection ess emojify doom-themes doom-modeline dockerfile-mode dired-single dired-hide-dotfiles counsel-projectile company-box command-log-mode buffer-move avy auto-package-update all-the-icons-ivy all-the-icons-dired))
 '(safe-local-variable-values
   '((org-ref-pdf-directory . "./pm_psych_pdfs/")
     (org-ref-default-bibliography . "pm_psych_refs.bib")
     (org-ref-bibliography-notes . "pm_psych_refs_notes.org")))
 '(warning-suppress-types '((use-package) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
