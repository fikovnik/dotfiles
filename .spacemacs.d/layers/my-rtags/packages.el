;;; packages.el --- my-layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Filip Krikava
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `mineo-rtags-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `mineo-rtags/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `mineo-rtags/pre-init-PACKAGE' and/or
;;   `mineo-rtags/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-rtags-packages
  '(cc-mode
    rtags
    company
    (company-rtags :toggle (configuration-layer/package-usedp 'company))
    flycheck-rtags
    helm-rtags))

(defun my-rtags/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . 'c++-mode)))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

(defun my-rtags/c-mode-common-hook ()
  (rtags-start-process-unless-running))

(defun my-rtags/flycheck-rtags-setup-hook ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))


(defun my-rtags/init-rtags ()
  (use-package rtags
    :load-path "/usr/local/share/emacs/site-lisp/rtags"
    :defer t
    :init
    (progn
      (setq rtags-autostart-diagnostics t
            rtags-completions-enabled t
            rtags-display-result-backend 'helm)
    :config
    (progn
      (rtags-enable-standard-keybindings))))

(defun my-rtags/post-init-rtags ()
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (add-hook 'c-mode-hook 'my-rtags/c-mode-common-hook)
      (add-hook 'c++-mode-hook 'my-rtags/c-mode-common-hook)))

(defun my-rtags/init-helm-rtags ()
  (use-package helm-rtags))

(defun my-rtags/init-company-rtags ()
  (use-package company-rtags))

(defun my-rtags/post-init-company-rtags ()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode
    "TAB" 'company-complete)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    "TAB" 'company-complete)

  (setq company-idle-delay nil)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends-c-mode-common))

(defun my-rtags/post-init-company ()
  (spacemacs|add-company-hook c-mode-common))

(defun my-rtags/init-flycheck-rtags ()
  (use-package flycheck-rtags))

(defun my-rtags/post-init-flycheck-rtags ()
  (add-hook 'c-mode-hook 'my-rtags/flycheck-rtags-setup-hook)
  (add-hook 'c++-mode-hook 'my-rtags/flycheck-rtags-setup-hook))

;;; packages.el ends here

