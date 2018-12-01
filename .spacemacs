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

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;;osx
     (auto-completion :variables
                      spacemacs-default-company-backends '(company-files company-capf)
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t)
     bibtex
     better-defaults
     colors
     (c-c++ :variables
            c-c++-backend 'lsp-cquery
            c-c++-lsp-sem-highlight-rainbow nil)
     docker
     dash
     emacs-lisp
     ess
     git
     helm
     html
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     latex
     markdown
     (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
     ;;notmuch
     (org :variables
          org-enable-github-support t
          org-projectile-file "~/Notes/TODO-projects.org")
     pdf
     racket
     ranger
     (scala :variables
            scala-indent:use-javadoc-style t
            scala-enable-eldoc t
            scala-auto-insert-asterisk-in-comments t
            scala-use-unicode-arrows t)
     spacemacs-org
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 50
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh")
     spell-checking
     sql
     syntax-checking
     version-control
     yaml

     ;; my layers
     my-layer
     my-ob-tmux
     ;;ace-link-notmuch
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(epresent base16-theme eterm-256color emamux)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

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

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

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

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

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
   dotspacemacs-editing-style 'emacs

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((agenda   . 5)
                                (recents  . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(base16-oceanicnext
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator utf-8 :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono for Powerline"
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

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
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

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
   dotspacemacs-frame-title-format "Emacs: %b [%m]"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

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

  ;; treat st terminal as xterm (cf. https://www.gnu.org/software/emacs/manual/html_node/elisp/Terminal_002dSpecific.html)
  (add-to-list 'term-file-aliases (quote ("st" . "xterm")))

  ;; fix keys in tmux
  (defadvice terminal-init-screen
      ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
      (before tmux activate)
    "Apply xterm keymap, allowing use of keys passed through tmux."
    ;; This is the elisp code that is run before `terminal-init-screen'.
    (if (getenv "TMUX")
        (let ((map (copy-keymap xterm-function-map)))
          (set-keymap-parent map (keymap-parent input-decode-map))
          (set-keymap-parent input-decode-map map))))
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

  ;; temporary until https://github.com/syl20bnr/spacemacs/issues/11640 gets fixed
  (ido-mode -1)

  ;; more colors in emacs terminals
  (add-hook 'term-mode-hook #'eterm-256color-mode)

  ;; make the cursor to stand out
  (setq evil-emacs-state-cursor '("magenta" (bar . 2)))

  ;; TODO: fix this
  ;; this is because it corrupts the modeline
  ;;(spaceline-toggle-hud-off)

  ;; (my-setup-input-decode-map)

  ;; slack
  ;; (slack-register-team
  ;;  :name "just-r.slack.com"
  ;;  :default t
  ;;  :client-id "krikava"
  ;;  :client-secret (string-trim (shell-command-to-string "/usr/bin/pass fit/just-r.slack.com | head -1"))
  ;;  :token (string-trim (shell-command-to-string "pass fit/just-r.slack.com | grep 'token:' | cut -d' ' -f 2"))
  ;;  :subscribed-channels '(general scala-implicit))

  (setq
   ;; wrap around window edges
   windmove-wrap-around t
   ;; allow pgup/pgdn to scroll to top and bottom
   scroll-error-top-bottom t
   ;; use system terminfo (tic -o /usr/share/terminfo <path-to-eterm-color.ti>)
   ;;system-uses-terminfo nil

   ;; c code
   c-default-style "stroustrup"
   c-basic-offset 4

   ;; so it does not ask "Keep current list of tags table also" which I do not want
   tags-add-tables nil

   ;; do not use clipboard
   x-select-enable-clipboard nil
   ;; ensime
   ensime-startup-notification nil
   )

  (setq-default
   tab-width 4
   indent-tabs-mode nil
   show-paren-delay 0
   )

  ;; magit
  (setq magit-repository-directories '(("~/Projects" . 1) ("~/Research/Projects" . 1)))

  ;; C++ for headers
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  ;; nice cursor
  (blink-cursor-mode nil)
  ;; replace highlighted text by typing
  (delete-selection-mode t)

  ;; TODO: does not work
  ;; smarter newline after "{"
  ;; (sp-local-pair 'prog-mode "{" nil :post-handlers '((my/create-newline-and-enter-sexp "RET")))
  ;; because ess-mode does not inherit prog-mode yet -
  ;; (sp-local-pair 'ess-mode "{" nil :post-handlers '((my/create-newline-and-enter-sexp "RET")))

  ;; R configuration
  (setq ess-tab-complete-in-script t
        ess-offset-arguments 'prev-line
        ess-nuke-trailing-whitespace-p t
        ess-build-tags-command "system(\"~/bin/rtags.R '%s' '%s'\")"
        ess-indent-with-fancy-comments nil
        ess-indent-offset 4
        ess-R-argument-suffix "="
        ess-R-font-lock-keywords '((ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:F&T . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators)
                                   (ess-fl-keyword:delimiters)
                                   (ess-fl-keyword:=)))

  (add-hook 'inferior-ess-mode-hook 'my/iess-mode-hook)
  (add-hook 'ess-mode-hook 'my/ess-mode-hook)

  ;; interpreters
  (eval-after-load "comint"
    '(progn
       (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
       (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
       (define-key comint-mode-map (kbd "C-k") 'kill-line)
       ;; enable completion
       (company-mode t)
       ;; also recommended for ESS use --
       (setq comint-scroll-to-bottom-on-output 'others)
       (setq comint-scroll-show-maximum-output t)
       ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
       (setq comint-scroll-to-bottom-on-input 'this)))

  (sp-use-smartparens-bindings)

  ;; SQL
  ;; (setq sql-mysql-optionsq (list "-P 6612"))
  ;; fix mysql prompt: https://unix.stackexchange.com/a/297320/118575
  ;; (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

  (spacemacs/set-leader-keys "bq" 'kill-buffer-and-window)

  ;; fix scrolling
  (xterm-mouse-mode 1)

  ;; (global-set-key [mouse-4] (lambda ()
  ;;                             (interactive)
  ;;                             (scroll-down 3)))
  ;; (global-set-key [mouse-5] (lambda ()
  ;;                             (interactive)
  ;;                             (scroll-up 3)))

  (setq
   pdf-misc-print-programm "/usr/bin/lpr"
   pdf-misc-print-programm-args (quote ("-o media=a4" "-o fitplot")))

  ;; --------------------------------------------------------------------------------
  ;; org-mode
  ;; --------------------------------------------------------------------------------
  (with-eval-after-load 'org
    (setq org-id-link-to-org-use-id 'create-if-interactive
          org-agenda-files '("~/Notes")
          org-directory "~/Notes"
          org-default-notes-file "~/Notes/Notes.org"
          org-startup-indented 1
          org-blank-before-new-entry '(((heading .  t) (plain-list-item . t)))
          org-log-reschedule 'time
          org-ellipsis "⤵"
          org-log-into-drawer t
          org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
    (setq org-capture-templates
          '(("t" "Todo"         entry (file+headline "~/Notes/TODO.org" "INBOX")  "* TODO %?\ncaptured on: %U\nfrom: %a\n%i")
            ("n" "Note"         entry (file+headline "~/Notes/Notes.org" "Notes") "* %?\ncaptured on: %U\nfrom: %a\n%i")
            ("j" "Journal"      entry (file+datetree "~/Notes/Journal.org")    "* %?\n%i")
            ("J" "Work Journal" entry (file+datetree "~/Notes/Work.org")  "* %?\n%i")
            ))
    )

  ;; --------------------------------------------------------------------------------
  ;; projectile
  ;; --------------------------------------------------------------------------------
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index"))

  ;; (with-eval-after-load 'org-agenda
  ;;    (require 'org-projectile)
  ;;    (setq org-agenda-files (append (org-projectile-todo-files) org-agenda-files)))

  (setq ranger-show-preview t
        ranger-show-literal nil
        ranger-cleanup-on-disable nil
        ranger-ignored-extensions '("mkv" "iso" "mp4")
        ranger-max-preview-size 20
        ranger-return-to-ranger t
        ranger-override-dired-mode t)

  ;; --------------------------------------------------------------------------------
  ;; e-mail
  ;; --------------------------------------------------------------------------------

  ;; notmuch
  (setq notmuch-search-oldest-first nil
        notmuch-message-deleted-tags '("+trash" "-inbox" "-unread")
        notmuch-show-all-multipart/alternative-parts nil
        notmuch-address-use-company t
        notmuch-hello-auto-refresh t
        notmuch-fcc-dirs nil
        notmuch-multipart/alternative-discouraged '("text/html" "multipart/related"))

  ;; queries
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :count-query "tag:inbox and tag:unread" :key "i")
          (:name "unread" :query "tag:unread not tag:trash" :key "u")
          (:name "flagged" :query "tag:flagged not tag:trash" :key "F")
          (:name "sent" :query "tag:sent not tag:trash" :key "t")
          (:name "drafts" :query "tag:draft not tag:trash" :key "d")
          (:name "all mail" :query "not tag:trash" :key "A")))

  ;; address selection with helm
  (setq notmuch-address-selection-function
        (lambda (prompt collection initial-input)
          (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

  ;; (defun notmuch-show-subject-tabs-to-spaces ()
  ;;   "Replace tabs with spaces in subject line."
  ;;   (goto-char (point-min))
  ;;   (when (re-search-forward "^Subject:" nil t)
  ;;     (while (re-search-forward "\t" (line-end-position) t)
  ;;       (replace-match " " nil nil))))

  ;; (add-hook 'notmuch-show-markup-headers-hook 'notmuch-show-subject-tabs-to-spaces)

  ;; (defun notmuch-show-header-tabs-to-spaces ()
  ;;   "Replace tabs with spaces in header line."
  ;;   (setq header-line-format
  ;;         (notmuch-show-strip-re
  ;;          (replace-regexp-in-string "\t" " " (notmuch-show-get-subject)))))

  ;; (add-hook 'notmuch-show-hook 'notmuch-show-header-tabs-to-spaces)

  ;; sending email
  (setq user-mail-address "krikava@gmail.com"
        user-full-name  "Filip Krikava"
        ;; where to get the account
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t
        ;;message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        send-mail-function 'sendmail-send-it
        sendmail-program "~/.local/bin/msmtpq"
        ;; html
        mm-text-html-renderer 'shr
        shr-width 80
        shr-use-colors nil)

  ;; org-ref
  (setq org-ref-default-bibliography '("~/Research/Resources/Papers/references.bib")
        org-ref-pdf-directory "~/Research/Resources/Papers"
        org-ref-bibliography-notes "~/Research/Resources/Papers/notes.org")

  ;; latex preview
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; synctex
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-method 'synctex)
  ;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; Latex
  ;; Use Skim on macOS to utilize synctex.
  ;; Confer https://mssun.me/blog/spacemacs-and-latex.html
  ;; AucTex recognizes some standard viewers, but the default view command
  ;; does not appear to sync.
  ;;(setq TeX-view-program-list
  ;;      '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
  ;;        ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;;(cond
  ;; ((spacemacs/system-is-mac) (setq TeX-view-program-selection '((output-pdf "Skim"))))
  ;; ((spacemacs/system-is-linux) (setq TeX-view-program-selection '((output-pdf "Okular"))))))

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
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
    (orgit org-brain ess request lsp-mode magit zeal-at-point yasnippet-snippets yaml-mode ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org tagedit symon string-inflection sql-indent spaceline-all-the-icons smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters racket-mode pug-mode prettier-js popwin persp-mode pcre2el password-generator paradox ox-gfm overseer org-ref org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-tmux noflet neotree nameless mwim mvn multi-term move-text move-dup mmm-mode meghanada maven-test-mode markdown-toc magit-svn magit-gitflow macrostep lsp-ui lsp-java lorem-ipsum link-hint julia-mode indent-guide impatient-mode ibuffer-projectile hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag groovy-mode groovy-imports gradle-mode google-translate google-c-style golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ ghub gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-rtags flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eterm-256color ess-R-data-view eshell-z eshell-prompt-extras esh-help epresent ensime emmet-mode emamux elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline dockerfile-mode docker disaster diminish diff-hl define-word cquery counsel-projectile company-web company-statistics company-rtags company-quickhelp company-lsp company-emacs-eclim company-c-headers company-auctex column-enforce-mode color-identifiers-mode clean-aindent-mode clang-format centered-cursor-mode ccls browse-at-remote base16-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
