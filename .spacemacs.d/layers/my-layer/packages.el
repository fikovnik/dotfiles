(defconst my-layer-packages
  '(move-dup)
  )

(defun org-insert-clipboard-image ()
  (interactive)
  (setq dir (concat (file-name-directory (buffer-file-name)) "resources"))
  (unless (file-directory-p dir) (make-directory dir))
  (setq filename
        (concat dir
                "/"
                (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
                "-"
                (format-time-string "%Y%m%d-%H%M%S")
                ".png"))
  (shell-command (concat " xclip -selection clipboard -t image/png -o > " filename))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my/toggle-emacs-mode ()
  (interactive)
  (if (bound-and-true-p holy-mode)
      (spacemacs/toggle-hybrid-mode)
    (spacemacs/toggle-holy-mode)))

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
        (if (= 0 (shell-command-on-region
                  (region-beginning) (region-end) "xsel -i -b"))
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
    (let*
        ((opt (prefix-numeric-value current-prefix-arg))
         (opt (cond
               ((=  1 opt) "b")
               ((=  4 opt) "p")
               ((= 16 opt) "s"))))
      (insert (shell-command-to-string (concat "xsel -o -" opt))))))

(defun my-setup-input-decode-map ()
  (defun add-escape-key-mapping-alist (escape-prefix key-prefix suffix-alist)
    (while suffix-alist
      (let ((escape-suffix (car (car suffix-alist)))
            (key-suffix (cdr (car suffix-alist))))
        (define-key input-decode-map (concat escape-prefix escape-suffix)
          (read-kbd-macro (concat key-prefix key-suffix))))
      (setq suffix-alist (cdr suffix-alist))))

  (setq nav-key-pair-alist
        '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>") ("D" . "<left>")
          ("H" . "<home>") ("F" . "<end>")))

  (add-escape-key-mapping-alist "\e[1;2" "S-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;3" "M-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;4" "M-S-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;5" "C-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;6" "C-S-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;7" "M-C-" nav-key-pair-alist)
  (add-escape-key-mapping-alist "\e[1;8" "M-C-S-" nav-key-pair-alist))



(defun kill-syntax (&optional arg)
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))

(defun backward-kill-syntax (&optional arg)
  (interactive)
  (kill-syntax -1))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	    (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defun my-layer/init-move-dup ()
  (require 'move-dup))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-h") 'my/toggle-emacs-mode)
    (define-key map [home] 'mwim-beginning-of-code-or-line)
    (define-key map [end] 'mwim-end-of-code-or-line)

    (define-key map (kbd "M-f") 'forward-same-syntax)
    (define-key map (kbd "M-b") (lambda () (interactive) (forward-same-syntax -1)))
    (define-key map (kbd "M-d") 'kill-syntax)
    (define-key map (kbd "M-<backspace>") 'backward-kill-syntax)

    (define-key map (kbd "M-S-<up>") 'md/duplicate-up)
    (define-key map (kbd "M-S-<down>") 'md/duplicate-down)
    (define-key map (kbd "M-<up>") 'md/move-lines-up)
    (define-key map (kbd "M-<down>") 'md/move-lines-down)

    ;;(define-key map (kbd "C-c C-w") 'my-cut-to-xclipboard)
    ;;(define-key map (kbd "C-c M-w") 'my-copy-to-xclipboard)
    ;;(define-key map (kbd "C-c C-y") 'my-paste-from-xclipboard)
    ;; this is just to make it compatible with terminal
    (define-key map (kbd "C-S-x") 'my-cut-to-xclipboard)
    (define-key map (kbd "C-S-c") 'my-copy-to-xclipboard)
    (define-key map (kbd "C-S-v") 'my-paste-from-xclipboard)
    
    ;; windmove
    (define-key map (kbd "C-x <left>")  'windmove-left)
    (define-key map (kbd "C-x <right>") 'windmove-right)
    (define-key map (kbd "C-x <up>")    'windmove-up)
    (define-key map (kbd "C-x <down>")  'windmove-down)

    (define-key map (kbd "C-x C-b") 'helm-mini)
    (define-key map (kbd "C-x C-r") 'ranger)
    (define-key map (kbd "M-x") 'helm-M-x)

    (define-key map (kbd "C-=") 'er/expand-region)
    ;; faster kill buffer
    (define-key map (kbd "C-x C-k") 'kill-this-buffer)

    ;; C-M-/ in terminal to toggle comment
    (define-key map (kbd "M-S-/") 'comment-or-uncomment-region-or-line)

    ;; indent
    (define-key map (kbd "C-M-\\") 'indent-region-or-buffer)

    ;; move
    (define-key map (kbd "C-M-i") 'evil-jump-forward)
    (define-key map (kbd "C-M-o") 'evil-jump-backward)

    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  :init-value t
  :lighter "my-keys")

(my-keys-minor-mode 1)
(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my/create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/iess-mode-hook ()
  (my/ess-mode-hook)
  (define-key inferior-ess-mode-map (kbd "C-k") 'kill-line))

(defun my/ess-mode-hook ()
  (ess-disable-smart-S-assign nil)
  (ess-set-style 'RStudio)
  (modify-syntax-entry ?_ "w"))

