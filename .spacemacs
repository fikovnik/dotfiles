;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
        ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
        ;; <M-m f e R> (Emacs style) to install them.
        ;; ----------------------------------------------------------------
        helm
        osx
        (auto-completion :variables
                         auto-completion-return-key-behavior 'complete
                         auto-completion-tab-key-behavior nil
                         auto-completion-enable-snippets-in-popup t
                         auto-completion-enable-help-tooltip t)
        better-defaults
        (ibuffer :variables ibuffer-group-buffers-by 'projects)
        emacs-lisp
        git
        ess
        html
        racket
        markdown
        docker
        sql

        my-layer
        ;;my-rtags
        ;;my-realgud
        my-polymode
        my-parinfer

        (latex :variables latex-build-command "LatexMk")
        (org :variables
             org-enable-github-support t
             org-projectile-file "~/Notes/Projects-TODO.org")
        (shell :variables
               shell-default-shell 'ansi-term
               shell-default-height 30
               shell-default-position 'full
               shell-default-term-shell "/usr/local/bin/zsh"
               shell-default-full-span nil)
        spell-checking
        syntax-checking
        theming
        version-control
        yaml
        )
        ;; List of additional packages that will be installed without being
        ;; wrapped in a layer. If you need some configuration for these
        ;; packages, then consider creating a layer. You can also put the
        ;; configuration in `dotspacemacs/user-config'.
        dotspacemacs-additional-packages
        '(
          simpleclip
          rainbow-blocks
          rainbow-mode
          dired-rainbow
          color-theme-solarized
        )
        ;; A list of packages that cannot be updated.
        dotspacemacs-frozen-packages '()
        ;; A list of packages that will not be installed and loaded.
        dotspacemacs-excluded-packages '(ess-R-object-popup)
        ;; Defines the behaviour of Spacemacs when installing packages.
        ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
        ;; `used-only' installs only explicitly used packages and uninstall any
        ;; unused packages as well as their unused dependencies.
        ;; `used-but-keep-unused' installs only the used packages but won't uninstall
        ;; them if they become unused. `all' installs *all* packages supported by
        ;; Spacemacs and never uninstall them. (default is `used-only')
        dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
       ;; This setq-default sexp is an exhaustive list of all the supported
       ;; spacemacs settings.
       (setq-default
        ;; allow to use right option to enter symbols
        mac-right-option-modifier nil
        ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
        ;; possible. Set it to nil if you have no way to use HTTPS in your
        ;; environment, otherwise it is strongly recommended to let it set to t.
        ;; This variable has no effect if Emacs is launched with the parameter
        ;; `--insecure' which forces the value of this variable to nil.
        ;; (default t)
        dotspacemacs-elpa-https t
        ;; Maximum allowed time in seconds to contact an ELPA repository.
        dotspacemacs-elpa-timeout 5
        ;; If non nil then spacemacs will check for updates at startup
        ;; when the current branch is not `develop'. Note that checking for
        ;; new versions works via git commands, thus it calls GitHub services
        ;; whenever you start Emacs. (default nil)
        dotspacemacs-check-for-update t
        ;; If non-nil, a form that evaluates to a package directory. For example, to
        ;; use different package directories for different Emacs versions, set this
        ;; to `emacs-version'.
        dotspacemacs-elpa-subdirectory nil
        ;; One of `vim', `emacs' or `hybrid'.
        ;; `hybrid' is like `vim' except that `insert state' is replaced by the
        ;; `hybrid state' with `emacs' key bindings. The value can also be a list
        ;; with `:variables' keyword (similar to layers). Check the editing styles
        ;; section of the documentation for details on available variables.
        ;; (default 'vim)
        dotspacemacs-editing-style 'emacs
        ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
        ;; `recents' `bookmarks' `projects' `agenda' `todos'."
        ;; List sizes may be nil, in which case
        ;; `spacemacs-buffer-startup-lists-length' takes effect.
        dotspacemacs-startup-lists '((recents . 5)
                                     (projects . 7))
        ;; True if the home buffer should respond to resize events.
        dotspacemacs-startup-buffer-responsive t
        ;; Default major mode of the scratch buffer (default `text-mode')
        dotspacemacs-scratch-mode 'text-mode
        ;; List of themes, the first of the list is loaded when spacemacs starts.
        ;; Press <SPC> T n to cycle to the next theme in the list (works great
        ;; with 2 themes variants, one dark and one light)
        dotspacemacs-themes '(spacemacs-dark
                              spacemacs-light)
        ;; If non nil the cursor color matches the state color in GUI Emacs.
        dotspacemacs-colorize-cursor-according-to-state t
        ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
        ;; quickly tweak the mode-line size to make separators look not too crappy.
        dotspacemacs-default-font '("DejaVu Sans Mono for Powerline Nerd Font"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1)
        ;; The leader key
        dotspacemacs-leader-key "SPC"
        ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
        ;; the key pairs C-i, TAB and C-m, RET.
        ;; Setting it to a non-nil value, allows for separate commands under <C-i>
        ;; and TAB or <C-m> and RET.
        ;; In the terminal, these pairs are generally indistinguishable, so this only
        ;; works in the GUI. (default nil)
        dotspacemacs-distinguish-gui-tab t
        ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
        dotspacemacs-remap-Y-to-y$ nil
        ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
        ;; there. (default t)
        dotspacemacs-retain-visual-state-on-shift t
        ;; If non-nil, J and K move lines up and down when in visual mode.
        ;; (default nil)
        dotspacemacs-visual-line-move-text nil
        ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
        ;; (default nil)
        dotspacemacs-ex-substitute-global nil
        ;; Name of the default layout (default "Default")
        dotspacemacs-default-layout-name "Default"
        ;; If non nil the default layout name is displayed in the mode-line.
        ;; (default nil)
        dotspacemacs-display-default-layout nil
        ;; If non nil then the last auto saved layouts are resume automatically upon
        ;; start. (default nil)
        dotspacemacs-auto-resume-layouts t
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
        ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
        dotspacemacs-helm-resize nil
        ;; if non nil, the helm header is hidden when there is only one source.
        ;; (default nil)
        dotspacemacs-helm-no-header nil
        ;; define the position to display `helm', options are `bottom', `top',
        ;; `left', or `right'. (default 'bottom)
        dotspacemacs-helm-position 'bottom
        ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
        ;; in all non-asynchronous sources. If set to `source', preserve individual
        ;; source settings. Else, disable fuzzy matching in all sources.
        ;; (default 'always)
        dotspacemacs-helm-use-fuzzy 'always
        ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
        ;; several times cycle between the kill ring content. (default nil)
        dotspacemacs-enable-paste-transient-state nil
        ;; Which-key delay in seconds. The which-key buffer is the popup listing
        ;; the commands bound to the current keystroke sequence. (default 0.4)
        dotspacemacs-which-key-delay 0.4
        ;; Which-key frame position. Possible values are `right', `bottom' and
        ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
        ;; right; if there is insufficient space it displays it at the bottom.
        ;; (default 'bottom)
        dotspacemacs-which-key-position 'bottom
        ;; If non nil a progress bar is displayed when spacemacs is loading. This
        ;; may increase the boot time on some systems and emacs builds, set it to
        ;; nil to boost the loading time. (default t)
        dotspacemacs-loading-progress-bar t
        ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
        ;; (Emacs 24.4+ only)
        dotspacemacs-fullscreen-at-startup nil
        ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
        ;; Use to disable fullscreen animations in OSX. (default nil)
        dotspacemacs-fullscreen-use-non-native nil
        ;; If non nil the frame is maximized when Emacs starts up.
        ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
        ;; (default nil) (Emacs 24.4+ only)
        dotspacemacs-maximized-at-startup nil
        ;; A value from the range (0..100), in increasing opacity, which describes
        ;; the transparency level of a frame when it's active or selected.
        ;; Transparency can be toggled through `toggle-transparency'. (default 90)
        dotspacemacs-active-transparency 90
        ;; A value from the range (0..100), in increasing opacity, which describes
        ;; the transparency level of a frame when it's inactive or deselected.
        ;; Transparency can be toggled through `toggle-transparency'. (default 90)
        dotspacemacs-inactive-transparency 90
        ;; If non nil show the titles of transient states. (default t)
        dotspacemacs-show-transient-state-title t
        ;; If non nil show the color guide hint for transient state keys. (default t)
        dotspacemacs-show-transient-state-color-guide t
        ;; If non nil unicode symbols are displayed in the mode line. (default t)
        dotspacemacs-mode-line-unicode-symbols nil
        ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
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
        ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
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
        ;; If non nil, advise quit functions to keep server open when quitting.
        ;; (default nil)
        dotspacemacs-persistent-server nil
        ;; List of search tool executable names. Spacemacs uses the first installed
        ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
        ;; (default '("ag" "pt" "ack" "grep"))
        dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
        ;; The default package repository used if no explicit repository has been
        ;; specified with an installed package.
        ;; Not used for now. (default nil)
        dotspacemacs-default-package-repository nil
        ;; Delete whitespace while saving buffer. Possible values are `all'
        ;; to aggressively delete empty line and long sequences of whitespace,
        ;; `trailing' to delete only the whitespace at end of lines, `changed'to
        ;; delete only whitespace for changed lines or `nil' to disable cleanup.
        ;; (default nil)
        dotspacemacs-whitespace-cleanup nil
        ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq
   ;; default window possition and size
   ;; TODO: in the center of a screen
   initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 60)))
)


(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; fixing keys
  (define-key input-decode-map "\e\e[A" [(meta up)])
  (define-key input-decode-map "\e\e[a" [(meta shift up)])
  (define-key input-decode-map "\e\e[b" [(meta shift down)])
  (define-key input-decode-map "\e\e[B" [(meta down)])

  (setq
   ;; fix the modeline separators
   powerline-default-separator 'utf-8
   ;; better scrolling
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
   scroll-step 1
   ;; wrap around window edges
   windmove-wrap-around t
   ;; allow pgup/pgdn to scroll to top and bottom
   scroll-error-top-bottom t
   ;; use system terminfo (tic -o /usr/share/terminfo <path-to-eterm-color.ti>)
   system-uses-terminfo nil

   ;; c code
   c-default-style "stroustrup"
   c-basic-offset 4

   ;; so it does not ask "Keep current list of tags tables also" which I do not want
   tags-add-tables nil)


  (setq-default
   tab-width 4
   indent-tabs-mode nil
   show-paren-delay 0)


  ;; C++ for headers
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  ;; nice cursor
  (blink-cursor-mode -1)

  ;; replace highlighted text by typing
  (delete-selection-mode 1)

  (simpleclip-mode 1)

  (global-prettify-symbols-mode t)
  (global-undo-tree-mode t)
  (global-auto-revert-mode t)

  (windmove-default-keybindings 'super)

  ;; smarter newline after "{"
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((my/create-newline-and-enter-sexp "RET")))
  ;; because ess-mode does not inherit prog-mode yet -
  (sp-local-pair 'ess-mode "{" nil :post-handlers '((my/create-newline-and-enter-sexp "RET")))

  ;; R configuration
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

  ;; smartparens bindings:
  ;;
  ;; ("C-M-f" . sp-forward-sexp)
  ;; ("C-M-b" . sp-backward-sexp)
  ;; ("C-M-d" . sp-down-sexp)
  ;; ("C-M-a" . sp-backward-down-sexp)
  ;; ("C-S-d" . sp-beginning-of-sexp)
  ;; ("C-S-a" . sp-end-of-sexp)
  ;; ("C-M-e" . sp-up-sexp)
  ;; ("C-M-u" . sp-backward-up-sexp)
  ;; ("C-M-n" . sp-next-sexp)
  ;; ("C-M-p" . sp-previous-sexp)
  ;; ("C-M-k" . sp-kill-sexp)
  ;; ("C-M-w" . sp-copy-sexp)
  ;; ("M-<delete>" . sp-unwrap-sexp)
  ;; ("M-<backspace>" . sp-backward-unwrap-sexp)
  ;; ("C-<right>" . sp-forward-slurp-sexp)
  ;; ("C-<left>" . sp-forward-barf-sexp)
  ;; ("C-M-<left>" . sp-backward-slurp-sexp)
  ;; ("C-M-<right>" . sp-backward-barf-sexp)
  ;; ("M-D" . sp-splice-sexp)
  ;; ("C-M-<delete>" . sp-splice-sexp-killing-forward)
  ;; ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
  ;; ("C-S-<backspace>" . sp-splice-sexp-killing-around)
  ;; ("C-]" . sp-select-next-thing-exchange)
  ;; ("C-M-]" . sp-select-next-thing)
  ;; ("C-M-SPC" . sp-mark-sexp)
  ;; ("M-F" . sp-forward-symbol)
  ;; ("M-B" . sp-backward-symbol)
  ;;
  (sp-use-smartparens-bindings)

  (sp-local-pair 'prog-mode "("
                 ")" :wrap "C-(")
  (sp-local-pair 'prog-mode "[" "]" :wrap "C-[")
  (sp-local-pair 'prog-mode "{" "}" :wrap "C-{")
  (setq sp-autoskip-closing-pair t)
  (setq sp-autodelete-pair t)

  ;; (sp-local-pair 'Org "=" "=" :trigger "=")
  ;; (sp-local-pair 'Org "~" "~" :trigger "~")

  ;; (SQL)
  ;; FIXME: just for now
  (setq sql-mysql-optionsq (list "-P 6612"))
  ;; fix mysql prompt: https://unix.stackexchange.com/a/297320/118575
  ;; (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

  (spacemacs/set-leader-keys
    "bq" 'kill-buffer-and-window)

  ;; fix scrolling
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 3)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 3)))

  ;; solarized
  (setq theming-modifications
        '((solarized
           ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
           ;; bright blue background, and all the others are in cream.
           ;; TODO: Change to use variables here. However, got error:
           ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
           (mode-line :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           ;; Make a really prominent helm selection line.
           (helm-selection :foreground "white" :background "red" :inverse-video nil)
           ;; dotspacemacs-colorize-cursor-according-to-state. to get this
           ;; to work you must set the variable
           ;; dotspacemacs-colorize-cursor-according-to-state to nil
           (cursor :background "#b58900")
           )))
  (set-terminal-parameter nil 'background-mode 'dark)
  (set-frame-parameter nil 'background-mode 'dark)
  (spacemacs/load-theme 'solarized)

  ;; mu4e
  (setq mu4e-maildir "~/Mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "mbsync krikava_at_gmail"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t)

  ;; latex preview
  ;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; Latex
  ;; Use Skim on macOS to utilize synctex.
  ;; Confer https://mssun.me/blog/spacemacs-and-latex.html
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-method 'synctex)
  ;; AucTex recognizes some standard viewers, but the default view command
  ;; does not appear to sync.
  (setq TeX-view-program-list
        '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
          ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (cond
   ((spacemacs/system-is-mac) (setq TeX-view-program-selection '((output-pdf "Skim"))))
   ((spacemacs/system-is-linux) (setq TeX-view-program-selection '((output-pdf "Okular")))))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Notes/Projects-TODO.org")))
 '(package-selected-packages
   (quote
    (rainbow-mode org-category-capture company-auctex auctex-latexmk auctex color-theme-solarized color-theme rainbow-blocks dired-rainbow dired-hacks-utils ox-gfm org-pomodoro alert log4e org-projectile org-present gntp org-download htmlize gnuplot ibuffer-projectile zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme sql-indent parinfer dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat polymode racket-mode faceup realgud test-simple loc-changes load-relative helm-rtags flycheck-rtags company-rtags xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help rtags ag yaml-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data simpleclip helm-company helm-c-yasnippet git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck diff-hl company-statistics company-quickhelp pos-tip company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete unfill mwim ess-smart-equals ess-R-object-popup ess-R-data-view ctable ess julia-mode smeargle orgit mmm-mode markdown-toc markdown-mode magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit magit-popup git-commit with-editor reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(tramp-syntax (quote default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
