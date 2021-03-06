;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   evil-escape-key-sequence "jk"

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(csv
     (helm :variables helm-no-header t)
     ;; html
     ;; colors
     ;; emoji
     ;; pdf
     docker
     yaml
     ;; csv
     (treemacs :variables
               treemacs-use-scope-type 'Perspectives
               treemacs-use-follow-mode t
               treemacs-icons-dired-mode t
               treemacs-use-git-mode 'simple
               treemacs-use-filewatch-mode t)
     ;; polymode
     ;; syntax-checking (syntax-checking :variables
     ;;                                  syntax-checking-enable-tooltips t)
     osx
     lsp (lsp :variables
              ;; lsp-clients-r-server-command ("R" "--no-save" "--no-restore" "--no-environ" "--no-site-file" "--slave" "-e" "languageserver::run()")
              lsp-ui-doc-enable nil
              lsp-ui-sideline-enable nil
              )
     ranger (ranger :variables
             ranger-override-dired-mode t
             ranger-show-hidden nil
             ranger-listing-dir-first t
             ranger-cleanup-on-disable t
             ranger-cleanup-eagerly t)
     bibtex (bibtex :variables
                    bibtex-autokey-year-length 4
                    bibtex-autokey-name-year-separator "-"
                    bibtex-autokey-year-title-separator "-"
                    bibtex-autokey-titleword-separator "-"
                    bibtex-autokey-titlewords 0
                    bibtex-completion-bibliography "~/dropbox/ITS_LIT_FAM/papers.bib"
                    bibtex-completion-library-path "~/dropbox/ITS_LIT_FAM/bibtex_pdfs/"
                    bibtex-completion-notes-path "~/dropbox/ITS_LIT_FAM/papers.org")
     git
     github
     ess (ess :variables
              ess-assign-key nil
              ess-indent-with-fancy-comments nil
              ess-use-tracebug nil
              ess-eval-visibly 'nowait
              ess-help-own-frame nil
              ess-help-reuse-window t
              ess-auto-width 'frame
              ess-R-font-lock-keywords
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
                (ess-R-fl-keyword:F&T))
              inferior-ess-r-font-lock-keywords
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
                (ess-R-fl-keyword:F&T))
              ess-ask-for-ess-directory nil
              inferior-R-args "--no-save --quiet"
              ess-S-quit-kill-buffers-p "t"
              display-buffer-alist
              `(("*R Dired"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (slot . -1)
                 (window-width . 0.33)
                 (reusable-frames . nil))
                ("*R"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-width . 0.5)
                 (dedicated . t)
                 (reusable-frames . nil))
                ("*Help"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.33)
                 (reusable-frames . nil)))
              )
     markdown (markdown :variables
                        markdown-command "pandoc"
                        markdown-live-preview-engine 'vmd)
     latex (latex :variables
                  latex-enable-folding t)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     org (org :variables
              org-agenda-span 10
              org-agenda-start-on-weekday nil
              org-agenda-start-day "-3d"
              org-todo-keywords '("TODO" "WAITING" "WISHING" "|" "DONE")
              org-todo-keyword-faces '(
                                       ("WAITING" . "systemBlueColor")
                                       ("WISHING" . "systemYellowColor")
                                       ("TODO" . "systemRedColor")
                                       ("DONE" . "systemGreenColor")
                                       )
              org-agenda-files '("~/dropbox/notes")
              org-refile-targets '(
                                   (org-agenda-files :maxlevel . 1)
                                   )
              org-capture-templates '(
                                      ("t" "todo [_todo.org tasks]" entry
                                       (file+headline "~/dropbox/notes/_todo.org" "Tasks")
                                       "* TODO %?")
                                      )
              org-default-notes-file "~/dropbox/notes/refile-beorg.org"
              org-startup-folded t
              org-latex-pdf-process "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"
              org-enable-github-support t
              org-enable-epub-support t
              org-enable-bootstrap-support t
              org-enable-sticky-header nil
              org-enable-reveal-js-support t)
     shell (shell :variables
                  shell-default-full-span nil
                  shell-default-shell 'vterm
                  shell-default-height 30
                  shell-default-position 'bottom)
     spell-checking (spell-checking :variables
                                    spell-checking-enable-auto-dictionary t
                                    enable-flyspell-auto-completion nil
                                    spell-checking-enable-by-default nil)
     auto-completion (auto-completion :variables
                                      auto-completion-return-key-behavior 'complete
                                      auto-completion-tab-key-behavior 'cycle
                                      auto-completion-complete-with-key-sequence nil
                                      auto-completion-complete-with-key-sequence-delay 0.1
                                      auto-completion-private-snippets-directory nil
                                      :disabled-for
                                      org
                                      git)
     themes-megapack
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(org-tree-slide)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(smartparens)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         monokai
                         zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(vim-powerline :separator arrow :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "home"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols 'display-graphic-p

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%U @ %S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq menu-bar-mode -1)

  ;; increase the amount of data emacs reads from the process to speed up lsp packages
  (setq read-process-output-max (* 1024 1024))

  (setq neo-theme 'classic)
  (setq vc-follow-symlinks t)
  ;; (setq reftex-default-bibliography '("~/dropbox/ITS_LIT_FAM/papers.bib"))
  ;; (setq org-ref-default-bibliography '("~/dropbox/ITS_LIT_FAM/papers.bib")
  ;;       org-ref-pdf-directory "~/dropbox/ITS_LIT_FAM/bibtex_pdfs/"
  ;;       org-ref-bibliography-notes "~/dropbox/ITS_LIT_FAM/papers.org")

  ;; open dired after selecting project in projectile
  (setq projectile-switch-project-action #'projectile-dired)
  ;; set paths for projects (search by restarting emacs)
  (setq projectile-project-search-path '("~/code" "~/icloud/works" "~/dotfiles"))

  ;; toggle visual line nav for all text mode
  ;; https://emacs.stackexchange.com/a/19364
  (spacemacs/toggle-truncate-lines-on)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  (add-hook 'ess-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;;;; modeline spacemacs toggles ;;;;
  (spacemacs/toggle-display-time-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  ;; (spacemacs/toggle-mode-line-battery-on)
  (spacemacs/toggle-tool-bar-off)
  (spacemacs/toggle-menu-bar-off)
  (spacemacs/toggle-yasnippet-off)

  ;; (spaceline-toggle-buffer-encoding-abbrev-off)
  ;; (spaceline-toggle-buffer-size-off)
  ;; (spaceline-toggle-process-off)

  ;; org-tree-slide configuration
  (setq org-tree-slide-slide-in-effect nil)

  ;; use all-the-icons for dired
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  ;; more ess settings

  (add-hook 'ess-mode-hook 'prettify-symbols-mode)

  (defun insert-pipe ()
    "Insert a %>%"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (define-key ess-mode-map (kbd "C-'") 'insert-pipe)

  (defun ess-edit-word-at-point ()
    (save-excursion
      (buffer-substring
       (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
       (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

  (defun ess-eval-word ()
    (interactive)
    (let ((x (ess-edit-word-at-point)))
      (ess-eval-linewise (concat x)))
    )

  (defun ess-glimpse-word ()
    (interactive)
    (let ((x (ess-edit-word-at-point)))
      (ess-eval-linewise (concat "glimpse(" x ")"))))

  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mT" "toggle")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "m=" "format")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mx" "text")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mb" "lsp/backend")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "ma" "activate")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mF" "folder")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mG" "peek")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mr" "refactor")
  (spacemacs/declare-prefix-for-mode 'ess-r-mode "mc" "chunks")

  (spacemacs/set-leader-keys-for-major-mode 'ess-r-mode
    "," 'ess-eval-line-and-step
    "e" 'ess-eval-paragraph-and-step
    "o" 'ess-eval-word
    ;; "og" 'ess-glimpse-word
    "R" 'ess-eval-region
    "cc" 'ess-eval-chunk
    "cd" 'ess-eval-chunk-and-step
    "ci" 'ess-insert-r-code-chunk
    "cr" 'ess-render-rmarkdown
    ;; TODO render from beginning up to current chunk
    "si" 'ess-interrupt
    "sr" 'inferior-ess-reload
    "ss" 'ess-switch-process
    )

  (defun ess-insert-r-code-chunk ()
    "Insert R Markdown code chunk."
    (interactive)
    (insert "```{r}\n")
    (save-excursion
      (insert "\n")
      (insert "```\n")))

  (defun ess-render-rmarkdown ()
    "Compile R markdown (.Rmd). Should work for any output type."
    (interactive)
    ;; Check if attached R-session
    (condition-case nil
        (ess-get-process)
      (error
       (ess-switch-process)))
    (let* ((rmd-buf (current-buffer)))
      (save-excursion
        (let* ((sprocess (ess-get-process ess-current-process-name))
               (sbuffer (process-buffer sprocess))
               (buf-coding (symbol-name buffer-file-coding-system))
               (R-cmd
                (format "rmarkdown::render(\"%s\", envir = new.env())"
                        buffer-file-name)))
          (message "Running rmarkdown on %s" buffer-file-name)
          (ess-execute R-cmd 'buffer nil nil)
          (switch-to-buffer rmd-buf)
          (ess-show-buffer (buffer-name sbuffer) nil)))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-modeline shrink-path zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler winum white-sand-theme which-key volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme ranger rainbow-delimiters railscasts-theme purple-haze-theme professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy paradox spinner ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-tree-slide org-ref pdf-tools key-chord ivy org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow magit-popup magit-gh-pulls madhat2r-theme lush-theme lorem-ipsum linum-relative link-hint light-soap-theme launchctl jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide ibuffer-projectile hydra lv hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-bibtex bibtex-completion parsebib helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache ht gh-md gandalf-theme fuzzy flyspell-correct-helm flyspell-correct flx-ido flx flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu ess-smart-equals ess-R-data-view ctable ess julia-mode espresso-theme eshell-z eshell-prompt-extras esh-help dumb-jump dracula-theme dockerfile-mode docker transient tablist s json-mode docker-tramp json-snatcher json-reformat django-theme diminish darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csv-mode company-statistics company-auctex company column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key biblio biblio-core dash badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons-dired all-the-icons memoize alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup)))
 '(safe-local-variable-values
   (quote
    ((org-reg-bibliography-notes . "pm_psych_refs_notes.org")
     (org-ref-pdf-directory . "pm_psych_pdfs/")
     (org-ref-default-bibliography . "pm_psych_refs.bib")
     (org-ref-pdf-directory . "./pm_psych_pdfs/")
     (org-ref-default-bibliography . "pm_psych_refs_notes.org")
     (org-ref-bibliography-notes . "pm_psych_refs_notes.org")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler winum white-sand-theme which-key volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme ranger rainbow-delimiters railscasts-theme purple-haze-theme professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy paradox spinner ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-tree-slide org-ref pdf-tools key-chord ivy org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow magit-popup magit-gh-pulls madhat2r-theme lush-theme lorem-ipsum linum-relative link-hint light-soap-theme launchctl jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide ibuffer-projectile hydra lv hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-bibtex bibtex-completion parsebib helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache ht gh-md gandalf-theme fuzzy flyspell-correct-helm flyspell-correct flx-ido flx flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu ess-smart-equals ess-R-data-view ctable ess julia-mode espresso-theme eshell-z eshell-prompt-extras esh-help dumb-jump dracula-theme dockerfile-mode docker transient tablist s json-mode docker-tramp json-snatcher json-reformat django-theme diminish darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csv-mode company-statistics company-auctex company column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key biblio biblio-core dash badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons-dired all-the-icons memoize alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
