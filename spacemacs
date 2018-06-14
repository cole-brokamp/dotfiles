;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   evil-escape-key-sequence "jk"
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-a0.7sk-for-lazy0.7-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     html
     helm
     emacs-lisp
     python
     csv
     (ranger :variables
             ranger-show-preview t)
     bibtex
     colors
     ;; tmux
     html
     emoji
     git
     games
     github
     ess
     markdown (markdown :variables
               markdown-live-preview-engine 'vmd)
     pdf-tools
     latex
     docker
     (osx :variables
          osx-use-option-as-meta t)
     ;; deft
     pandoc
     search-engine
     twitter
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t)
     (shell :variables
            shell-default-full-span nil
            shell-default-shell 'multi-term
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary nil
                     enable-flyspell-auto-completion nil
                     spell-checking-enable-by-default nil)
     shell-scripts
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     themes-megapack
     yaml
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((bookmarks . 10)
                                (recents . 12)
                                (projects . 12))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "home"
   dotspacemacs-display-default-layout t
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.3
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-config ()
  (setq neo-theme 'classic)

;;;; version control ;;;;
  (setq vc-follow-symlinks t)

;;;; org ;;;;
  (setq org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d")
  (setq org-todo-keywords
        '((sequence "TODO" "WAITING" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("WAITING" . "systemYellowColor")
          ("TODO" . "systemRedColor")
          ("DONE" . "systemGreenColor")))
  (setq org-agenda-files '("~/dropbox/notes"))
  (setq org-startup-folded t)
  (setq org-ref-default-bibliography "~/dropbox/ITS_LIT_FAM/bib_test.bib"
        org-ref-pdf-directory "~/dropbox/ITS_LIT_FAM/bibtex_pdf/"
        org-ref-bibliography-notes "~/dropbox/ITS_LIT_FAM/bib_test.org")
  (setq org-default-notes-file '"~/dropbox/notes/refile-beorg.org")

;;;; ess ;;;;

  ;; don't replace underscore with assignment operator in ess
  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-toggle-underscore nil)))

  (setq ess-ask-for-ess-directory nil)
  ;; setq inferior-R-args '((sequence "--no-save" "--quiet"))
  (setq ess-S-quit-kill-buffers-p t)
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-default-style 'Rstudio)
  (setq ess-eval-visibly 'nowait)

  ;;; mappings
  ; C-RET to send paragraph
  ; C-SHIFT-RET to send link

;;;; ranger ;;;;

  (setq ranger-cleanup-on-disable t)
  (setq ranger-parent-depth 3)
  (setq ranger-override-dired-mode t)


;;;; ??? ;;;

  (add-hook 'doc-view-mode-hook 'auto-revert-mode) ;; perform full-document previews in Latex major mode
  (setq spaceline-org-clock-p t)
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(package-selected-packages
   (quote
    (twittering-mode ranger engine-mode company-auctex auctex-latexmk auctex typit mmt sudoku pacmacs 2048-game yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic vmd-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data rainbow-mode rainbow-identifiers color-identifiers-mode ox-twbs ox-reveal ox-gfm ess-smart-equals ess-R-data-view ctable ess julia-mode dockerfile-mode docker json-mode docker-tramp json-snatcher json-reformat zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme fish-mode farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csv-mode company-shell color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme emoji-cheat-sheet-plus company-emoji yaml-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache helm-themes helm-swoop helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag flyspell-correct-helm ace-jump-helm-line xterm-color unfill shell-pop mwim multi-term git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete smeargle reveal-in-osx-finder pbcopy pandoc-mode ox-pandoc ht osx-trash osx-dictionary orgit org-ref pdf-tools key-chord tablist org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc markdown-mode magit-gitflow launchctl htmlize helm-bibtex parsebib gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit magit-popup git-commit ghub let-alist with-editor deft biblio biblio-core ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f dash s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed async aggressive-indent adaptive-wrap ace-window ace-link avy)))
 '(paradox-github-token t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
