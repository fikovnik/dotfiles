;; ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ;; Resources:
;; ;; - https://github.com/hlissner/doom-emacs/blob/develop/modules
;; ;; - https://github.com/zaiste/.doom.d/
;; ;;   - the screencast guy
;; ;; - https://github.com/rschmukler/doom.d
;; ;; - https://github.com/ar1a/dotfiles/blob/master/emacs/.doom.d
;; ;; - https://github.com/forrestchang/.doom.d
;; ;; - and https://github.com/search?l=Emacs+Lisp&o=desc&q=%22.doom.d%22&s=stars&type=Repositories


(setq display-line-numbers-type t
      deft-directory (expand-file-name "~/Notes")
      deft-recursive t
      evil-want-fine-undo t
      org-directory (expand-file-name "~/Notes")    ; must be set before org is loaded
      user-full-name "Filip Krikava"
      user-mail-address "krikava@gmail.com"
      visual-order-cursor-movement t
      which-key-idle-delay 0.3)

(setq-default evil-shift-width 2
              tab-width 2)

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,"
      doom-font (font-spec :family "dejavu sans mono" :size 18)
      doom-big-font (font-spec :family "dejavu sans mono" :size 18)
      doom-theme 'doom-one)

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar window-number modals matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

 (doom-modeline-def-modeline 'special
    '(bar window-number modals matches buffer-info-simple buffer-position selection-info)
    '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number modals buffer-default-directory)
    '(misc-info mu4e github debug battery " " major-mode process)))

(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)

  (setq web-mode-markup-indent-offset 2  ; Indentation
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil ; disbale adding "" after an =
        web-mode-auto-close-style 2))

(after! lsp
  ;; These take up a lot of space on my big font size
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-signature-render-all nil))

(after! dash
  (dash-enable-font-lock))

(after! evil
  ;; this makes the Y/P work the same as in vim
  (evil-put-command-property 'evil-yank-line :motion 'evil-line))

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
        org-ellipsis "↴"
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

(use-package! org-mru-clock
  :after org
  :commands (org-mru-clock-in org-mru-clock-select-recent-task)
  :custom
  (org-mru-clock-how-many 100)
  (org-mru-clock-completing-read #'ivy-completing-read)
  (org-mru-clock-keep-formatting t))

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

  (map! :map ivy-minibuffer-map
        "C-z" #'ivy-dispatching-done
        "C-w" #'ivy-yank-word
        "C-'" #'ivy-avy
        "<left>" #'counsel-up-directory
        "<backtab>" #'counsel-up-directory
        "<right>" #'my--ivy-enter-directory-or-insert
        "TAB" #'my--enter-directory-or-insert)

  (setq ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t)

  (minibuffer-depth-indicate-mode 1))

(after! company
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-selection-wrap-around t)

  (map!
   :map company-active-map
   "C-j" #'company-complete-selection
   "RET" nil
   [return] nil))

(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets"))

;; ----------------------------------------------------------------------------
;; GLOBAL MAP
;; ----------------------------------------------------------------------------

(map!
 :n "C-h" nil
 :i "C-x C-s"  #'my-save-buffer-and-switch-to-normal-mode
 :i "C-x s" #'company-yasnippet
 :g "C-s"   #'swiper-isearch
 :i "C-k"   #'kill-visual-line
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

;; ----------------------------------------------------------------------------
;; TOGGLE MAP
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
