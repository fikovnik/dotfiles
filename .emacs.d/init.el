(require 'org)

(let ((config-file-org (expand-file-name "config.org" user-emacs-directory))
      (config-file-el (expand-file-name "config.el" user-emacs-directory))
      (config-file-elc (expand-file-name "config.elc" user-emacs-directory)))

  (cond ((file-exists-p config-file-elc) (load config-file-elc))
        ((file-exists-p config-file-el) (load-file config-file-el))
        ((file-exists-p config-file-org)
         (org-babel-tangle-file config-file-org config-file-el "emacs-lisp")
         (load-file config-file-el))))
