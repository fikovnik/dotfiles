;;; packages.el --- ace-link-notmuch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Filip Krikava <krikava@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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
;; added to `ace-link-notmuch-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ace-link-notmuch/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ace-link-notmuch/pre-init-PACKAGE' and/or
;;   `ace-link-notmuch/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ace-link-notmuch-packages
  '((ace-link-notmuch :location local)))

(defun ace-link-notmuch/init-ace-link-notmuch ()
  (use-package ace-link-notmuch
    :commands (ace-link-notmuch-hello ace-link-notmuch-show)
    :init
    (progn
      (with-eval-after-load 'notmuch
        (define-key notmuch-hello-mode-map "o" 'ace-link-notmuch-hello)
        (define-key notmuch-show-mode-map "o" 'ace-link-notmuch-show)))))

;;; packages.el ends here
