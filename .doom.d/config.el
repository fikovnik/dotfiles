;; ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ;; Resources:
;; ;; - https://github.com/hlissner/doom-emacs/blob/develop/modules
;; ;; - https://github.com/zaiste/.doom.d/
;; ;;   - the screencast guy
;; ;; - https://github.com/rschmukler/doom.d
;; ;; - https://github.com/ar1a/dotfiles/blob/master/emacs/.doom.d
;; ;; - https://github.com/forrestchang/.doom.d
;; ;; - and https://github.com/search?l=Emacs+Lisp&o=desc&q=%22.doom.d%22&s=stars&type=Repositories

;; TODO my terminal mode - make it a private package
;; (xclip-mode -1)

(setq display-line-numbers-type t
      deft-directory (expand-file-name "~/Notes")
      deft-recursive t
      doom-localleader-key ","
      doom-localleader-alt-key "M-,"
      doom-font (font-spec :family "dejavu sans mono" :size 18)
      doom-big-font (font-spec :family "dejavu sans mono" :size 18)
      doom-theme 'doom-one
      evil-want-fine-undo t
      org-directory (expand-file-name "~/Notes")    ; must be set before org is loaded
      projectile-project-search-path '("~/Projects" "~/Research" "~/Sync/Projects" "~/Sync/Research")
      user-full-name "Filip Krikava"
      user-mail-address "krikava@gmail.com"
      visual-order-cursor-movement t
      which-key-idle-delay 0.3)

(setq-default evil-shift-width 2
              tab-width 2)

(after! agda2
  (set-lookup-handlers! 'agda2-mode
    :definition #'agda2-goto-definition-keyboard)

  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

  (map! :after agda2-mode
        :map agda2-mode-map
        :localleader
        (:prefix "g"
          "G" nil
          "b" #'agda2-go-back
          "d" #'agda2-goto-definition-keyboard)))

(after! company
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-selection-wrap-around t)

  (map!
   :map company-active-map
   "C-j" #'company-complete-selection ; has to be done explicitly because of evil
   "RET" nil
   [return] nil))

(after! dash
  (dash-enable-font-lock))

(after! doom-modeline
  (setq doom-modeline-modal-icon nil)

  (doom-modeline-def-modeline 'main
    '(bar window-number modals matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number modals matches buffer-info-simple buffer-position selection-info)
    '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number modals buffer-default-directory)
    '(misc-info mu4e github debug battery " " major-mode process)))

(after! ediff
  ;; from https://emacs.stackexchange.com/a/21460/26020
  ;; Check for org mode and existence of buffer
  (defun f-ediff-org-showhide (buf command &rest cmdargs)
    "If buffer exists and is orgmode then execute command"
    (when buf
      (when (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
        (save-excursion (set-buffer buf) (apply command cmdargs)))))

  (defun f-ediff-org-unfold-tree-element ()
    "Unfold tree at diff location"
    (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

  (defun f-ediff-org-fold-tree ()
    "Fold tree back to top level"
    (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

  (add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
  (add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree))

(after! evil
  ;; this makes the Y/P work the same as in vim
  (evil-put-command-property 'evil-yank-line :motion 'evil-line))

(after! ivy
  (defun my--ivy-is-directory-p ()
    (and
     (> ivy--length 0)
     (not (string= (ivy-state-current ivy-last) "./"))
     (not (null (ivy-expand-file-if-directory (ivy-state-current ivy-last))))))

  (defun my--ivy-enter-directory-or-insert ()
    (interactive)
    (if (my--ivy-is-directory-p)
        (counsel-down-directory)
      (progn
        (let ((last-input (ivy--input)))
          (ivy-insert-current)
          (when (string= last-input (ivy--input))
            (ivy-call))))))

  (defun my--ivy-other-window-action (file-name)
    "Opens the current candidate in another window."
    (select-window
     (with-ivy-window
       (find-file-other-window (expand-file-name file-name (ivy-state-directory ivy-last)))
       (selected-window))))

  ;; M-o show action list
  ;; C-' ivy-avy
  ;; C-M-m ivy-call - does the action, but does not exit ivy
  ;; C-M-o like M-o but does not quit
  ;; C-M-n/p cobines C-n/p and C-M-m
  (map! :map ivy-minibuffer-map
        "C-j" #'ivy-alt-done ; has to be explicit because of evil
        "C-z" #'ivy-dispatching-done
        "C-w" #'ivy-yank-word
        "C-'" #'ivy-avy
        "<left>" #'counsel-up-directory
        "<backtab>" #'counsel-up-directory
        "<right>" #'my--ivy-enter-directory-or-insert
        "TAB" #'my--ivy-enter-directory-or-insert)

  (setq ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t)

  (mapc
   (lambda (cmd)
     (ivy-add-actions
      cmd
      '(("O" my--ivy-other-window-action "open in other window"))))
   '(counsel-find-file counsel-recentf counsel-fzf counsel-dired doom/find-file-in-private-config))

  (minibuffer-depth-indicate-mode 1))

(after! lsp
  ;; These take up a lot of space on my big font size
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-signature-render-all nil))

(after! magit
  (defconst my-dotfiles-git-dir (expand-file-name "~/.dotfiles"))

  (defun my--dotfiles-remove-magit-config (&optional kill)
    (setq magit-git-global-arguments
          (remove (format "--work-tree=%s" (getenv "HOME")) magit-git-global-arguments))
    (setq magit-git-global-arguments
          (remove (format "--git-dir=%s" my-dotfiles-git-dir) magit-git-global-arguments))
    (advice-remove 'magit-mode-bury-buffer #'my--dotfiles-remove-magit-config))

  ;; TODO make this work even if magit has not been loaded yet
  (defun my-dotfiles-magit ()
    (interactive)
    (when (and (boundp 'magit-git-global-arguments)
               (file-exists-p my-dotfiles-git-dir))
      (let ((home (getenv "HOME")))
        (add-to-list 'magit-git-global-arguments
                     (format "--work-tree=%s" home))
        (add-to-list 'magit-git-global-arguments
                     (format "--git-dir=%s" my-dotfiles-git-dir))
        (advice-add 'magit-mode-bury-buffer :after #'my--dotfiles-remove-magit-config)
        (magit-status-setup-buffer home)))))

(after! org
  (setq org-agenda-files '("~/Notes/Journal")
        org-blank-before-new-entry '(((heading .  t) (plain-list-item . t)))
        org-capture-templates
        '(("t" "Todo"
           entry (file+headline "~/Notes/Journal/TODO.org" "INBOX")  "* TODO %?\ncaptured on: %U\nfrom: %a\n%i")
          ("n" "Note"
           entry (file+headline "~/Notes/Journal/Notes.org" "Notes") "* %?\ncaptured on: %U\nfrom: %a\n%i"))
        org-catch-invisible-edits 'smart
        org-confirm-babel-evaluate nil
        org-bullets-bullet-list '("⁖")
        org-ellipsis " ․․․"
        org-id-link-to-org-use-id 'create-if-interactive
        org-image-actual-width nil
        org-imenu-depth 8
        org-latex-prefer-user-labels t
        org-log-done t
        org-log-done-with-time nil
        org-log-into-drawer t
        org-log-reschedule 'time
        org-refile-active-region-within-subtree t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((nil :maxlevel . 6) (org-agenda-files :maxlevel . 7))
        org-refile-use-cache nil
        org-refile-use-outline-path 'file
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-startup-with-inline-images t
        org-startup-indented t
        org-special-ctrl-a/e t
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-show-context-detail
        '((agenda . local)
          (bookmark-jump . lineage)
          (isearch . tree) ; I want to see more info when looking at tree
          (default . ancestors)))

  (defun my-org-babel-remove-result-buffer ()
    "Remove results from every code block in buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (org-babel-remove-result))))

  (defadvice org-archive-subtree (around my-org-archive-subtree activate)
    "Archives all items under the current heading"
    (let ((org-archive-location
           (if (save-excursion (org-back-to-heading)
                               (> (org-outline-level) 1))
               ((concat ) (car (split-string (or )g-archive-location "::"))
                "::* "
                (car (org-get-outline-path)))
             org-archive-location)))
      ad-do-it))

    (map! :map org-mode-map
          :localleader
          "d" nil
          (:prefix ("j" . "journal")
            "j" #'org-journal-new-entry
            "s" #'org-journal-search-forever)
          (:prefix ("d" . "dates")
            "e" #'org-evaluate-time-range
            "d" #'org-deadline
            "s" #'org-schedule
            "t" #'org-time-stamp
            "T" #'org-time-stamp-inactive)))

(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets"))

(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)

  (setq web-mode-markup-indent-offset 2  ; Indentation
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil ; disbale adding "" after an =
        web-mode-auto-close-style 2))

;; -----------------------------------------------------------------------------
;; Definitions of my packages
;; -----------------------------------------------------------------------------

(use-package! flyspell-correct-ivy
  :after (flyspell ivy)
  :commands (flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :bind
  (:map flyspell-mode-map
    ("M-'" . flyspell-correct-wrapper)))

(use-package! org-journal
  :after org
  :preface
  ;; from https://github.com/bastibe/org-journal#journal-capture-template
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  :commands (org-journal-new-entry org-journal-search-forever)
  :custom
  (org-journal-date-format "%A, %B %d %Y")
  (org-journal-file-format "Journal-%Y.org")
  (org-journal-dir "~/Sync/Notes/Journal/")
  (org-journal-file-type 'yearly)
  :init
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry (function org-journal-find-location)
                  "** %(format-time-string org-journal-time-format)%?")))

(defun my-save-buffer-and-switch-to-normal-mode ()
  (interactive)
  (save-buffer)
  (evil-force-normal-state))

(defun my-kill-buffer-and-window ()
  (interactive)
  (if (> (count-windows) 1)
      (kill-buffer-and-window)
    (kill-buffer)))

(defun my-switch-to-messages-buffer (&optional arg)
    "Switch to the `*Messages*' buffer. If prefix argument ARG is
given, switch to it in an other, possibly new window."
    (interactive "P")
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (if arg
          (switch-to-buffer-other-window (current-buffer))
        (switch-to-buffer (current-buffer)))))

(defun my-copy-to-xclipboard ()
  (interactive)
  (if (use-region-p)
      (if (not (display-graphic-p))
          (letrec ((s (buffer-substring-no-properties (region-beginning) (region-end)))
                   (s-length (+ (* (length s) 3) 2)))
            (if (<= s-length 16384) ; magic number set to the same as ESC_BUF_SIZ of suckless termial (st.c)
                (progn
                  (send-string-to-terminal (concat "\e]52;c;"
                                                   (base64-encode-string (encode-coding-string s 'utf-8) t)
                                                   "\07"))
                  (message "Yanked region to terminal clipboard")
                  (deactivate-mark))
              (message "Selection too long (%d) to send to terminal." s-length)))
        (if (= 0 (shell-command-on-region (region-beginning) (region-end) "xsel -i -b"))
            (message "Yanked region to X-clipboard")
          (error "Is program `xsel' installed?")))
    (message "Nothing to yank to terminal clipboard")))

(defun my-cut-to-xclipboard ()
  (interactive)
  (my-copy-to-xclipboard)
  (kill-region (region-beginning) (region-end)))

(defun my-paste-from-xclipboard ()
  "Uses shell command `xsel -o' to paste from x-clipboard. With
one prefix arg, pastes from X-PRIMARY, and with two prefix args,
pastes from X-SECONDARY."
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (letrec
        ((opt (prefix-numeric-value current-prefix-arg))
         (opt (cond
               ((=  1 opt) "b")
               ((=  4 opt) "p")
               ((= 16 opt) "s"))))
(insert (shell-command-to-string (concat "xsel -o -" opt))))))

(use-package! org-mru-clock
  :after org
  :commands (org-mru-clock-in org-mru-clock-select-recent-task)
  :custom
  (org-mru-clock-how-many 100)
  (org-mru-clock-completing-read #'ivy-completing-read)
  (org-mru-clock-keep-formatting t))

(use-package! super-save
  :unless noninteractive
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'winum-select-window-by-number)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode 1))

;; ----------------------------------------------------------------------------
;; GLOBAL MAP
;; ----------------------------------------------------------------------------

(map!
 :nievg "C-/" #'comment-or-uncomment-region
 :n "C-h" nil
 :i "C-x C-s"  #'my-save-buffer-and-switch-to-normal-mode
 :i "C-x s" #'company-yasnippet
 :g "C-s"   #'swiper-isearch
 :i "C-k"   #'kill-visual-line
 :g "C-S-X" #'my-cut-to-xclipboard
 :g "C-S-C" #'my-copy-to-xclipboard
 (:when (display-graphic-p)
  ; The paste shortcut (=C-S-V=) we only want in GUI. When running in terminal it
  ; is better to use the terminal paste since it will be a [[https://cirw.in/blog/bracketed-paste][bracketed paste]].
  :g "C-S-V" #'my-paste-from-xclipboard)
 :n "M-y"   #'counsel-yank-pop
 :g "M-1"   #'winum-select-window-1
 :g "M-2"   #'winum-select-window-2
 :g "M-3"   #'winum-select-window-3
 :g "M-4"   #'winum-select-window-4
 :g "M-5"   #'winum-select-window-5
 :g "M-6"   #'winum-select-window-6
 :g "M-7"   #'winum-select-window-7
 :g "M-8"   #'winum-select-window-8
 :g "M-9"   #'winum-select-window-9
 :g "M-0"   #'winum-select-window-0
 (:when (featurep! :ui workspaces)
   :g "C-1"   #'+workspace/switch-to-0
   :g "C-2"   #'+workspace/switch-to-1
   :g "C-3"   #'+workspace/switch-to-2
   :g "C-4"   #'+workspace/switch-to-3
   :g "C-5"   #'+workspace/switch-to-4
   :g "C-6"   #'+workspace/switch-to-5
   :g "C-7"   #'+workspace/switch-to-6
   :g "C-8"   #'+workspace/switch-to-7
   :g "C-9"   #'+workspace/switch-to-8
   :g "C-0"   #'+workspace/switch-to-final))

;; ----------------------------------------------------------------------------
;; LEADER MAP
;; ----------------------------------------------------------------------------

(map! :leader
      "X" nil
      :desc "Capture" "C" #'org-capture
      :desc "Time clock" "T" #'org-mru-clock-in
      :desc "Recent clocks" "R" #'org-mru-clock-select-recent-task
      :desc "Journal entry" "J" #'org-journal-new-entry
      (:prefix ("w" . "window")
        ;; the following are mapped to moving to window function
        ;; these are no needed as they are already mapped to C-w h / leader h
        ;; plus C-h is useful to see the rest of the mapping in which-key
        "C-h" nil
        "C-j" nil  ; ""
        "C-k" nil  ; ""
        "C-l" nil) ; ""
      (:prefix ("b" . "buffer")
        :desc "Switch to message buffer" "M"  #'my-switch-to-messages-buffer
        :desc "Kill buffer and window" "d" #'my-kill-buffer-and-window)
      (:prefix ("g". "git")
        :desc "My dotfiles status" "M" #'my-dotfiles-magit)
      (:prefix ("t" . "toggle")
        :desc "Auto-fill mode" "W" #'auto-fill-mode))

;; ;; Here are some additional functions/macros that could help you configure Doom:
;; ;;
;; ;; - `load!' for loading external *.el files relative to this one
;; ;; - `use-package' for configuring packages
;; ;; - `after!' for running code after a package has loaded
;; ;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;; ;;   looks when you load packages with `require' or `use-package'.
;; ;; - `map!' for binding new keys
;; ;;
;; ;; To get information about any of these functions/macros, move the cursor over
;; ;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; ;; This will open documentation for it, including demos of how they are used.
;; ;;
