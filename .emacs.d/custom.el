(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-keys (quote (97 115 100 102 103 104 106 107 108)) t)
 '(aw-scope (quote frame) t)
 '(custom-safe-themes
   (quote
    ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" default)))
 '(iqa-user-init-file "~/.emacs.d/config.org")
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1) t)
 '(org-agenda-files (quote ("~/Notes/Journal")))
 '(org-blank-before-new-entry (quote (((heading . t) (plain-list-item . t)))))
 '(org-bullets-bullet-list (quote ("•")) t)
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+headline "~/Notes/Journal/TODO.org" "INBOX")
      "* TODO %?
captured on: %U
from: %a
%i")
     ("n" "Note" entry
      (file+headline "~/Notes/Journal/Notes.org" "Notes")
      "* %?
captured on: %U
from: %a
%i")
     ("j" "Journal" entry
      (file+datetree "~/Notes/Journal/Journal.org")
      "* %?
%i")
     ("J" "Work Journal" entry
      (file+datetree "~/Notes/Journal/Work.org")
      "* %?
%i"))))
 '(org-default-notes-file "~/Notes/Journal/Notes.org")
 '(org-directory "~/Notes")
 '(org-ellipsis "↴")
 '(org-id-link-to-org-use-id (quote create-if-interactive))
 '(org-image-actual-width nil)
 '(org-latex-prefer-user-labels t)
 '(org-log-done t)
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote time))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9))))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-startup-indented 1)
 '(org-startup-with-inline-images t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))
 '(package-selected-packages (quote (use-package)))
 '(smartrep-mode-line-active-bg "grey75")
 '(smartrep-mode-line-string-activated "[SR]")
 '(which-key-idle-delay 0.7))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
