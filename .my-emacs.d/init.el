;;; -*- lexical-binding: t -*-

;; This will time immediate initialization to play the _under a second startup_
;; game. Not that it would really matter since I have instance of Emacs being
;; launched by systemd.
(defconst emacs-start-time (current-time)
  "Time when emacs has started.")

;; allow more memory between GC cycle (default is 780kB) at startup
(let ((gc-cons-threshold most-positive-fixnum))

  (require 'package)

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/") t)

  (setq-default load-prefer-newer t
                package-enable-at-startup nil
                use-package-always-ensure t ; Auto-download package if not exists
                use-package-always-defer t ; Always defer load package to speed up startup
                use-package-verbose nil ; Don't report loading details
                use-package-expand-minimally t  ; make the expanded code as minimal as possible
                use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

  ;; init.el file debugging
  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
        use-package-expand-minimally t))

  (package-initialize)

  ;; refresh the packages descriptions
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;; install newest org - it is important that this happens before any org function has been loaded.
  ;; Otherwise autoload org functions will mess up the installation.
  (unless (package-installed-p 'org-plus-contrib)
    (package-install 'org-plus-contrib))

  ;; TODO support byte compilation
  (let ((config-file-org (expand-file-name "config.org" user-emacs-directory))
        (config-file-el (expand-file-name "config.el" user-emacs-directory)))

    (unless (file-newer-than-file-p config-file-el config-file-org)
      (require 'org)
      (org-babel-tangle-file config-file-org config-file-el "emacs-lisp"))

    (load-file config-file-el)))
