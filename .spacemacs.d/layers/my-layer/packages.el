(defconst my-layer-packages
  '()
  )

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

;; duplicate lines
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
       (interactive "p")
       (let (beg end (origin (point)))
         (if (and mark-active (> (point) (mark)))
             (exchange-point-and-mark))
         (setq beg (line-beginning-position))
         (if mark-active
             (exchange-point-and-mark))
         (setq end (line-end-position))
         (let ((region (buffer-substring-no-properties beg end)))
           (dotimes (i arg)
             (goto-char end)
             (newline)
             (insert region)
             (setq end (point)))
           (goto-char (+ origin (* (length region) arg) arg)))))

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

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [home] 'mwim-beginning-of-code-or-line)
    (define-key map [end] 'mwim-end-of-code-or-line)
    (define-key map (kbd "M-f") 'forward-same-syntax)
    (define-key map (kbd "M-b") (lambda () (interactive) (forward-same-syntax -1)))
    (define-key map (kbd "M-d") 'kill-syntax)
    (define-key map (kbd "M-<backspace>") 'backward-kill-syntax)
    (define-key map [M-S-down] 'duplicate-current-line-or-region)
    (define-key map [M-up] 'move-text-up)
    (define-key map [M-down] 'move-text-down)

    (define-key map (kbd "C-x C-b") 'helm-mini)
    (define-key map (kbd "M-x") 'helm-M-x)

    (define-key map (kbd "C-=") 'er/expand-region)

    ;; faster kill buffer
    (define-key map (kbd "C-x C-k") 'kill-this-buffer)
    (define-key map (kbd "s-w") 'spacemacs/frame-killer)

    (define-key map (kbd "s-n")
      (lambda ()
        (interactive)
        (select-frame (make-frame))
        (spacemacs/new-empty-buffer)
        (spacemacs/toggle-maximize-buffer)))

    (define-key map (kbd "C-/") 'comment-or-uncomment-region-or-line)

    ;; indent
    (define-key map (kbd "C-M-\\") 'indent-region-or-buffer)

    ;; move
    (define-key map (kbd "C-M-i") 'evil-jump-forward)
    (define-key map (kbd "C-M-o") 'evil-jump-backward)

    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  :init-value t
  :lighter " my-keys")

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
  (ess-disable-smart-underscore nil)
  (ess-set-style 'RStudio)
  (setq ess-tab-complete-in-script t
        ess-offset-arguments 'prev-line
        ess-nuke-trailing-whitespace-p t
        ess-build-tags-command "system(\"~/bin/rtags.R '%s' '%s'\")"
        ess-indent-with-fancy-comments nil
        ess-indent-offset 4
        ess-R-argument-suffix "=")
  (modify-syntax-entry ?_ "w")
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t))))

