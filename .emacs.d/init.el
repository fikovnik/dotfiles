(defvar my-init-el-start-time (current-time) "Time when init.el was started")

;; turn off early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; turn off messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "krikava")
(setq initial-scratch-message "")

;; setup package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; pin packages
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))

;; bootstrap use-package
;; - install use-package if it's not already installed
;; - use-package is used to configure the rest of the packages
(unless package--initialized (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

(message "Initialized in %.2fs" (float-time (time-subtract (current-time) my-init-el-start-time)))
