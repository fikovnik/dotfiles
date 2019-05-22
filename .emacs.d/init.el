(require 'org)

(let ((config-file (expand-file-name "config.org" user-emacs-directory))
      (init-file (expand-file-name "init.el" user-emacs-directory)))

  (org-babel-tangle-file config-file init-file "emacs-lisp")
  (load-file init-file))
