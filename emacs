; -*- mode: emacs-lisp; -*-

;; did you know?
; use M-n in any ivy window to automatically insert the word under the cursor as the search term

;; TODO make M-x show recent commands up top

;; basic ==========================================================

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
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

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq display-time-load-average-threshold 5
      display-time-day-and-date t)
(display-time)

(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)

; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

; fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 130)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 130)

;; increase the amount of data emacs reads from the process to speed up lsp packages
(setq read-process-output-max (* 1024 1024))

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
  
; TODO make SPC leader key work in motion state
(use-package general
  :config
  (general-create-definer cole/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer cole/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :non-normal-prefix "C-,")


  (cole/leader-keys
    "SPC" '(counsel-M-x :which-key "M-x")
    "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "/" 'swiper-all
    "'" '(term :which-key "shell")
    ";" '(evilnc-comment-or-uncomment-lines :which-key "comment operator")
    ;; "/" '(:which-key "search project")
    "a" '(:ignore t :which-key "applications")
    ;; "ad" docker
    "ac" 'calendar
    ;; "ab" '(ivy-bibtex :which-key "bibtex")
    "ab" '(ivy-bibtex-with-local-bibliography :which-key "bibtex (local bib)") ; auto uses bib file from \bibliography in files!
    "aB" '(ivy-bibtex :which-key "bibtex (global bib)") ; auto uses bib file from \bibliography in files!
    "o" '(:ignore t :which-key "org")
    "oc" '(counsel-org-capture "org capture")
    "oa" 'org-agenda
    ":" '(shell-command :which-key "shell command")
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "quit")
    "qr" '(restart-emacs :which-key "restart")
    "f" '(:ignore t :which-key "files")
    "fs" '(save-buffer :which-key "save")
    "fS" '(evil-write-all :which-key "save all")
    "ff" '(counsel-find-file :which-key "find file")
    "fD" '(delete-file :which-key "delete file")
    "fr" '(revert-buffer :which-key "reload file from disk")
    "b" '(:ignore t :which-key "buffers")
    "bb" '(ivy-switch-buffer :which-key "switch to buffer")
    "bB" '(counsel-switch-buffer :which-key "switch buffer with preview")
    "bf" '(reveal-in-osx-finder :which-key "show buffer in finder")
    "bd" '(kill-current-buffer :which-key "delete buffer")
    "bm" '(cole/switch-to-messages-buffer :which-key "messages buffer")
    "bs" '(cole/switch-to-scratch-buffer :which-key "scratch buffer")
    "bn" '(cole/buffer-file-name :which-key "copy buffer filename")
    "c" '(:ignore t :which-key "compile")
    "cc" '(counsel-compile :which-key "compile")
    "ck" '(kill-compilation :which-key "kill compilation")
    "cd" '(cole/show-hide-compilation-window :which-key "show/hide compilation window")
    "i" '(:ignore t :which-key "insert")
    "ie" '(emojify-insert-emoji :which-key "insert emoji")
    "io" '(newline-and-indent :which-key "open line")
    "ij" '(evil-collection-unimpaired-insert-newline-below :which-key "insert line below")
    "ik" '(evil-collection-unimpaired-insert-newline-above :which-key "insert line above")
    "h" '(:ignore t :which-key "help")
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key
    "hm" 'describe-mode
    "hb" '(general-describe-keybindings :which-key "key bindings")
    "hp" 'describe-package
    "h." 'display-local-help
    "j" '(:ignore t :which-key "jump")
    "jd" '(dired-jump :which-key "dired-jump")
    "n" '(:ignore t :which-key "notes")
    "nn" '((lambda () (interactive) (dired "~/dropbox/notes")) :which-key "notes")
    "nt" '((lambda () (interactive) (find-file "~/dropbox/notes/_todo.org")) :which-key "_todo.org")
    "nw" '((lambda () (interactive) (find-file "~/dropbox/notes/wiki.org")) :which-key "wiki.org")
    "ns" '((lambda () (interactive) (find-file "~/dropbox/notes/students.org")) :which-key "students.org")
    "nr" '((lambda () (interactive) (find-file "~/dropbox/notes/refile-beorg.org")) :which-key "refile-beorg.org")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tf" '(toggle-frame-fullscreen :which-key "full screen")
    "tl" '(toggle-truncate-lines :which-key "truncate lines")
    "tL" '(visual-line-mode :which-key "visual line mode")
    "tm" '(toggle-frame-maximized :which-key "maximize screen")
    "w" '(:ignore t :which-key "windows")
    "wd" '(evil-window-delete :which-key "delete window")
    "w/" '(cole/split-window-right-and-focus :which-key "split right")
    "w-" '(cole/split-window-below-and-focus :which-key "split down")
    "wh" '(evil-window-left :which-key "move left")
    "wl" '(evil-window-right :which-key "move right")
    "wj" '(evil-window-down :which-key "move down")
    "wk" '(evil-window-up :which-key "move up")
    "wf" '(make-frame :which-key "make into frame")
    "wm" '(cole/toggle-maximize-buffer :which-key "maximize window")
    "w=" '(balance-windows-area :which-key "equal window areas")
    "e" '(:ignore t :which-key "emacs")
    "ed" '(cole/find-user-init-file :which-key "open emacs dotfile")
    "s" '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper")
    "sr" '(query-replace :which-key "search and replace")
    "sR" '(query-replace-regexp :which-key "search and replace (regex)")
    ))

(use-package reveal-in-osx-finder)



;; do a transient state for window sizing and rotating
;; evil-window-{increase,decrease}-{width,height}

;; shell ----------------------------------

(with-eval-after-load 'shell
  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

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


(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :custom (all-the-icons-ivy-file-commands '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

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

(use-package all-the-icons)

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
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
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
  (evil-org-agenda-set-keys)
  ;; TODO make local leader only work in desired modes
  (cole/local-leader-keys
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
   "d" '(:ignore t :which-key "date")
   "ds" '(org-schedule :which-key "schedule")
   "dd" '(org-deadline :which-key "deadline")
   "dt" '(org-time-stamp-inactive :which-key "timestamp (inactive)")
   "s" '(:ignore t :which-key "subtree")
   "sa" '(org-archive-subtree :which-key "archive subtree")
   "sr" '(org-refile :which-key "refile")
   ))

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


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  :custom
  (smooth-scroll-margin 4)
  )

;; ensure full $PATH makes it into emacs
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; TODO set default shell so it doesn't prompt me everytime

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-go --all --classify --group-directories-first --dired --human-readable")
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

;; TODO use macOS emoji font for emoticons
;; (when (fboundp 'set-fontset-font)
;;   (set-fontset-font "fontset-default"
;;                     '(#x1F600 . #x1F64F)
;;                     (font-spec :name "Apple Color Emoji") nil 'prepend))

;; TODO SPC doesn't work in dired mode (have to use Ctl-SPC), fix this!

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
    "pk" '(projectile-kill-buffers :which-key "kill project buffers")
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
    "ld" '(eyebrowse-close-window-config :which-key "close layout")
    "lR" '(eyebrowse-rename-window-config :which-key "rename layout")
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
  ))

(use-package evil-magit
  :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

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

  (setq org-todo-keyword-faces '(("WAITING" . "systemBlueColor")
                                 ("WISHING" . "systemYellowColor")
                                 ("TODO" . "systemRedColor")
                                 ("DONE" . "systemGreenColor")))
)

(use-package org
  :hook (org-mode . cole/org-mode-setup)
  :custom
  (org-ellipsis "▾")
  (org-agenda-files '("~/dropbox/notes"))
  (org-default-notes-file '("~/dropbox/notes/refile-beorg.org"))
  (org-todo-keywords '("TODO" "WAITING" "WISHING" "|" "DONE"))
  (org-agenda-span 10)
  (org-agenda-start-day "-3d")
  (org-agenda-start-on-weekday nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-startup-folded t)
  (org-capture-templates '(
                   ("t" "todo [_todo.org tasks]" entry
                   (file+headline "~/dropbox/notes/_todo.org" "Tasks")
                    "* TODO %?")
                   ))
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers) ; save org buffers after refiling
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers) ; save org buffers after archiving
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

;; (use-package ivy-bibtex
;;   :after ivy
;;   :custom
;;   (bibtex-completion-bibliography '("~/dropbox/its_lit_fam/bib.bib"
;; 				    "~/dropbox/its_lit_fam/papers.org"
;; 				    ("~/dropbox/grants/pm_psych_refs_notes.org" . "~/dropbox/grants/pm_psych_refs.bib")
;; 				    "~/dropbox/grants/r01_nlm_degauss/r01_nlm_degauss.org")))
;; (setq bibtex-completion-library-path '("/path1/to/pdfs" "/path2/to/pdfs"))
;; (setq bibtex-completion-notes-path "/path/to/notes.org")
;; bibtex-completion-cite-default-command ... set this to ivy?
;; By default, helm-bibtex and ivy-bibtex prompt for pre- and postnotes for the citation. This can be switched off by setting the variable bibtex-completion-cite-prompt-for-optional-arguments to nil.
  

;; packages todo ============================================
;; TODO
 ;;   bibtex (bibtex :variables
 ;;                    bibtex-autokey-year-length 4
 ;;                    bibtex-autokey-name-year-separator "-"
 ;;                    bibtex-autokey-year-title-separator "-"
 ;;                    bibtex-autokey-titleword-separator "-"
 ;;                    bibtex-autokey-titlewords 0
 ;;                    bibtex-completion-bibliography "~/dropbox/ITS_LIT_FAM/papers.bib"
 ;;                    bibtex-completion-library-path "~/dropbox/ITS_LIT_FAM/bibtex_pdfs/"
 ;;                    bibtex-completion-notes-path "~/dropbox/ITS_LIT_FAM/papers.org")

;; https://github.com/jrblevin/markdown-mode
 ;; latex
 ;; something for make?
 ;; auto-completion

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
(defun cole/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . cole/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

;; (use-package lsp-treemacs
;;   :after lsp)

;; ess ------------------------------------------------------------------------

;; (use-package ess
;;   :custom
;;   (ess-eval-visibly 'nowait)
;;   (ess-use-tracebug nil)
;;   (ess-indent-with-fancy-comments nil)
;;   (ess-default-style 'RStudio)
;;   (ess-help-own-frame 'one)
;;   (ess-ask-for-ess-directory nil)
;;   (inferior-R-args "--no-save --quiet")
;;   (ess-S-quit-kill-buffers-p "t")
;;   (ess-auto-width 'frame)
;;   (comint-scroll-to-bottom-on-input t)
;;   (comint-scroll-to-bottom-on-output t)
;;   (comint-move-point-for-output t)
;;   (comint-scroll-show-maximum-output t)
;;   (ess-R-font-lock-keywords
;;    '((ess-R-fl-keyword:keywords   . t)
;;      (ess-R-fl-keyword:constants  . t)
;;      (ess-R-fl-keyword:modifiers  . t)
;;      (ess-R-fl-keyword:fun-defs   . t)
;;      (ess-R-fl-keyword:assign-ops . t)
;;      (ess-R-fl-keyword:%op%       . t)
;;      (ess-fl-keyword:fun-calls)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators . t)
;;      (ess-fl-keyword:delimiters)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T)))
;;   (inferior-ess-r-font-lock-keywords
;;    '((ess-S-fl-keyword:prompt . t)
;;      (ess-R-fl-keyword:messages . t)
;;      (ess-R-fl-keyword:modifiers . t)
;;      (ess-R-fl-keyword:fun-defs . t)
;;      (ess-R-fl-keyword:keywords . t)
;;      (ess-R-fl-keyword:assign-ops)
;;      (ess-R-fl-keyword:constants . t)
;;      (ess-fl-keyword:matrix-labels)
;;      (ess-fl-keyword:fun-calls)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators)
;;      (ess-fl-keyword:delimiters)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T)))
;;   :config
;;   (add-hook 'ess-mode-hook 'prettify-symbols-mode)
;;   )

;; (defun cole/insert-pipe ()
;;   "Insert a %>%"
;;   (interactive)
;;   (just-one-space 1)
;;   (insert "%>%")
;;   (reindent-then-newline-and-indent))
;; (define-key ess-mode-map (kbd "C-'") 'insert-pipe)

;; (defun cole/ess-edit-word-at-point ()
;;   (save-excursion
;;     (buffer-substring
;;      (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
;;      (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

;; (defun cole/ess-eval-word ()
;;   (interactive)
;;   (let ((x (cole/ess-edit-word-at-point)))
;;     (ess-eval-linewise (concat x)))
;;   )

;; (defun cole/ess-glimpse-word ()
;;   (interactive)
;;   (let ((x (cole/ess-edit-word-at-point)))
;;     (ess-eval-linewise (concat "glimpse(" x ")"))))

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

;; hydras ====================================================

(use-package hydra)

(defhydra cole/present ()
  "presentation"
  ("t" org-tree-slide-mode "toggle slide mode")
  ("n" org-tree-slide-move-next-tree "next")
  ("p" org-tree-slide-move-previous-tree "previous")
  ("q" nil "quit" :exit t))

(defhydra cole/scale-text (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(cole/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ivy lsp-ui lsp-mode ess dockerfile-mode flyspell-correct-ivy flyspell-correct yaml-mode which-key visual-fill-column use-package undo-fu smooth-scrolling reveal-in-osx-finder restart-emacs rainbow-delimiters osx-trash org-tree-slide org-bullets ivy-rich ivy-hydra ivy-bibtex helpful general forge exec-path-from-shell evil-org evil-nerd-commenter evil-magit evil-escape evil-collection emojify doom-themes doom-modeline dired-single dired-hide-dotfiles counsel-projectile command-log-mode all-the-icons-ivy all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
