;;; editing.el --- Editing and navigation commands and their key bindings  -*- lexical-binding: t -*-


(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 3))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 3))

(defun join-line-backward ()
  (interactive)
  (join-line -1)
  (indent-for-tab-command))


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
    (if (and (= (current-column) 0)
             (/= (line-beginning-position) beg))
        (setq end (line-beginning-position))
      (setq end (line-end-position)))
    (let ((region (buffer-substring beg end)))
      (dotimes (i arg)
        (goto-char end)
        (if (/= (current-column) 0)
            (newline))
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun narrow-to-region-indirect (beg end name)
  "Open a new named indirect buffer of the current buffer,
narrowed to region [BEG, END]."
  (interactive "r\nsname of narrowed buffer: ")
  (let ((new-buff
         (make-indirect-buffer (current-buffer)
                               (generate-new-buffer-name (concat "narrow-" name))
                               t)))
    (switch-to-buffer new-buff nil t)
    (narrow-to-region beg end)
    (deactivate-mark)
    (remove-overlays)))

(defun unfill ()
  "Maximally undo the effect of a fill"
  (interactive)

  (let ((left (save-excursion
		(min (progn (beginning-of-line) (point))
		     (progn (backward-paragraph) (point)))))
	(right (make-marker)))
    (save-excursion
      (set-marker right (max (progn (end-of-line) (point))
			     (progn (forward-paragraph) (point))))
      (goto-char left) (forward-line 1)
      (while (< (point) right)
	(end-of-line) (delete-char 1) (just-one-space))
      (delete-char -1) (insert "\n"))))

(defun debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error is now %s" (if debug-on-error "ON" "OFF")))

(defun truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "truncate-lines is now %s" (if truncate-lines "ON" "OFF")))

(defun indent-buffer ()
  (interactive)
  (indent-region 0 (buffer-size) nil))

(defun set-tab-width (width)
  (interactive "p")
  (set-variable 'tab-width width)
  (recenter))

(defun insert-time-stamp ()
  (interactive)
  (insert (current-time-string)))

(defun selective-display (column)
  "set selective display fold everything greater than the current column, or toggle off if active"
  (interactive "p")
  (set-selective-display
   (cond (selective-display nil)
         ((> column 1) (+ column 1))
         ((current-column) (+ (current-column) 1))
         (t 1))))

(defun bring-sexp (p)
  "Bring the sexp on the right of the current enclosing sexp to this location"
  (interactive "d")
  (require 'subr-x) ;; for string-trim
  (let ((to-insert
         (save-excursion
           (up-list)
           (when (char-equal (following-char) ?\;)
             (forward-char))
           (let ((left (point)))
             (forward-sexp)
             (string-trim (delete-and-extract-region left (point)))))))
    (insert to-insert)
    (indent-region p (point))))

(defadvice kill-buffer (around immortal-scratch-buffer activate)
  (let* ((buf-or-name (ad-get-arg 0))
         (buffer (if buf-or-name
                     (get-buffer-create buf-or-name)
                   (current-buffer)))
         (name (buffer-name buffer))
         (immortal (and (string= name "*scratch*")
                        (not (buffer-file-name buffer)))))
    (when (and ad-do-it immortal)
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (emacs-lisp-mode)
      (bury-buffer))))

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point. When no file name given,
   inserts the name of the file of the current buffer.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (if (eq filename "")
      (setq filename (buffer-file-name (window-buffer (minibuffer-selected-window)))))

  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))



(defun save-word-as-kill (arg) (interactive "p") (save-motion-as-kill 'forward-word arg))
(defun save-sexp-as-kill (arg) (interactive "p") (save-motion-as-kill 'forward-sexp arg))
(defun forward-save-sexp-as-kill (arg) (interactive "p") (save-sexp-as-kill arg))
(defun backward-save-sexp-as-kill (arg) (interactive "p") (save-sexp-as-kill (- arg)))
(defun backward-save-word-as-kill (arg) (interactive "p") (save-word-as-kill (- arg)))

(defvar up-list-stack ())
(defvar up-list-stack-max 50)
(make-variable-buffer-local 'up-list-stack)

(defun up-list-backward-sexp ()
  (interactive)
  (setq up-list-stack (cons (point) up-list-stack))
  (when (> (length up-list-stack) up-list-stack-max)
    (setcdr (nthcdr (1- up-list-stack-max) up-list-stack) nil))
  (up-list)
  (backward-sexp))

(defun up-list-pop ()
  (interactive)
  (when up-list-stack
    (goto-char (car up-list-stack))
    (setq up-list-stack (cdr up-list-stack))))

(defun find-tag-at-point ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))




(defun open-line-better (arg)
  (interactive "P")
  (when arg
    (next-line arg))
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)]   'gcm-scroll-up)

(global-set-key [f2] 'call-last-kbd-macro)
(global-set-key [\M-f2] 'apply-macro-to-region-lines)
(global-set-key "\C-hn" 'man)
(global-set-key "\C-x$" 'selective-display)
(with-eval-after-load 'tab-line
  (define-key tab-line-mode-map (kbd "C-x <right>") nil)
  (define-key tab-line-mode-map (kbd "C-x <left>") nil))
(global-set-key [f4] 'projectile-ag)
(global-set-key [f9] 'recompile)
(global-set-key [M-f9] 'switch-to-compilation)
(global-set-key "\C-j" 'join-line-backward)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-=" 'er/expand-region)
(global-set-key "\M-+" 'er/contract-region)
(global-set-key "\M-]" 'find-tag-at-point)
(global-set-key "\M-[" 'pop-tag-mark)
(global-set-key "\M-B" 'bring-sexp)
(global-set-key "\M-r" 'repeat)
(global-set-key "\M-j" 'ace-jump-word-mode)
(global-set-key "\C-o" 'open-line-better)
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key [M-delete] 'kill-word)
(global-set-key "\C-xni" 'narrow-to-region-indirect)
(global-unset-key [(control wheel-up)])
(global-unset-key [(control wheel-down)])

(global-set-key (kbd "s-M-<right>") 'next-buffer)
(global-set-key (kbd "s-M-<left>") 'previous-buffer)


(defvar personal-map (make-sparse-keymap))
(define-key global-map [(control c)] personal-map)

(define-key personal-map "w" 'agent-shell-send-dwim)
(define-key personal-map "d" 'duplicate-current-line-or-region)
(define-key personal-map "\C-p" 'compile)
(define-key personal-map "p" 'projectile-command-map)
(define-key personal-map "a" 'magit-status)
(define-key personal-map "bb" 'magit-blame)
(define-key personal-map "o" 'occur)
(define-key personal-map "'" 'iedit-mode) ; Ctrl-c '
(define-key personal-map "0" 'insert-console-log)
(define-key personal-map "i" 'iedit-mode)
(define-key personal-map "j" 'json-reformat-region)
(define-key personal-map "r" (lambda () (interactive) (revert-buffer t t t)))
(define-key personal-map "fg" 'lgrep)
(define-key personal-map "\C-q" 'fill-region)
(define-key personal-map "q" 'unfill)
(define-key personal-map "mb" 'bind-last-kdb-macro)
(define-key personal-map "md" 'global-unset-key)
(define-key personal-map "l" 'font-lock-fontify-buffer)
(define-key personal-map "ff" 'find-file-at-point)
(define-key personal-map "fi" 'insert-file-name)
(define-key personal-map "fb" 'browse-url-at-point)
(define-key personal-map "fd" 'find-name-dired)
(define-key personal-map "fa" 'ag)
(define-key personal-map "t" 'auto-revert-tail-mode)
(define-key personal-map "v" 'view-mode)
(define-key personal-map "y" (lambda () (interactive) (flyspell-mode) (flyspell-buffer)))
(define-key personal-map [up] 'up-list-backward-sexp)
(define-key personal-map [down] 'up-list-pop)
(define-key personal-map "-" 'text-scale-decrease)
(define-key personal-map "=" 'text-scale-increase)
(define-key personal-map [tab] 'set-tab-width)
(define-key personal-map "h" 'highlight-phrase)
(define-key personal-map "\C-h" 'highlight-regexp)
(define-key personal-map "c" 'calculator)
(define-key personal-map "n" 'flycheck-next-error)
(define-key personal-map "," 'lsp-find-references)

                                        ;(define-key ghostel-mode-map "l", 'ghostel-clear)

(defvar fzf-map (make-sparse-keymap))
(define-key fzf-map "f" 'fzf)
(define-key fzf-map "d" 'fzf-find-file-in-dir)
(define-key fzf-map "b" 'fzf-switch-buffer)
(define-key personal-map "fz" fzf-map)
(define-key personal-map "fj" #'jump-to-code)

(fset 'indent-riffle [tab down])
(if window-system (define-key global-map "\C-z" 'indent-riffle))

(defadvice isearch-repeat (after isearch-autowrap)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-autowrap)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-autowrap)
    (ad-activate 'isearch-repeat)))

(defun isearch-forward-symbol-at-point-now ()
  (interactive)
  (isearch-forward-symbol-at-point)
  (ad-enable-advice 'isearch-repeat 'after 'isearch-autowrap)
  (ad-activate 'isearch-repeat)
  (isearch-repeat-forward)
  (ad-disable-advice 'isearch-repeat 'after 'isearch-autowrap)
  (ad-activate 'isearch-repeat))

(global-set-key "\M-'" 'isearch-forward-symbol-at-point-now)

(eval-after-load "iedit"
  '(progn
     (define-key iedit-mode-keymap "\r" 'iedit-mode)))

(defun unmark-sexp ()
  "Undo the last `mark-sexp' extension."
  (interactive)
  (mark-sexp (if (> (mark) (point)) -1 1) t))
(keymap-global-set "C-M-<return>" #'unmark-sexp)
