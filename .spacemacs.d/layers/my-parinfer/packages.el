;;; packages.el --- my-parinfer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Filip Krikava <krikava@kathmandu.local>
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
;; added to `my-parinfer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-parinfer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-parnifer/pre-init-PACKAGE' and/or
;;   `my-parinfer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-parinfer-packages
  '(parinfer)
  "The list of Lisp packages required by the my-parifer layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-parinfer/parinfer-mode-hook ()
  (smartparens-strict-mode (not parinfer-mode))
  (smartparens-mode (not parinfer-mode))
  (show-smartparens-mode (not parinfer-mode)))

(defun my-parinfer/init-parinfer ()
  (use-package parinfer
    :defer t
    :diminish parinfer-mode
    :init
    (progn
      (spacemacs|add-toggle parinfer
        :evil-leader "tP"
        :mode parinfer-mode
        :documentation "Enable Parinfer Mode.")
      (setq parinfer-extensions '(defaults pretty-parens evil smart-tab smart-yank)))
    :bind
    (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right-or-complete)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-M-q" . parinfer--reindent-sexp)
        ("C-," . parinfer-toggle-mode)
        :map parinfer-region-mode-map
        ;;     ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left))
    :config
    (progn
      (add-hook 'parinfer-mode-hook 'my-parinfer/parinfer-mode-hook)
      (parinfer-strategy-add 'default 'newline-and-indent)
      (parinfer-strategy-add 'instantly
        '(parinfer-smart-tab:dwim-right
          parinfer-smart-tab:dwim-right-or-complete
          parinfer-smart-tab:dwim-left)))))

;;; packages.el ends here
