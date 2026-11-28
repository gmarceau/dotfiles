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

(defalias 'debug-on-error #'toggle-debug-on-error)
(defalias 'truncate-lines #'toggle-truncate-lines)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

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

(defun immortal-scratch-buffer ()
  "When *scratch* is killed, replace it with a fresh, buried one.
Runs from `kill-buffer-query-functions': renames the buffer being
killed out of the way, creates the new *scratch*, and lets the kill
proceed."
  (when (and (equal (buffer-name) "*scratch*") (not buffer-file-name))
    (rename-buffer "*scratch*<killed>" t)
    (let ((scratch (get-buffer-create "*scratch*")))
      (with-current-buffer scratch (emacs-lisp-mode))
      (bury-buffer scratch)))
  t)
;; Depth 90: run last, so no later query function can cancel the kill
;; after the replacement exists.
(add-hook 'kill-buffer-query-functions #'immortal-scratch-buffer 90)

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
  (if (string-empty-p filename)
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





(defun open-line-better (arg)
  (interactive "P")
  (when arg
    (forward-line (prefix-numeric-value arg)))
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

;;; Settings

;; Options (fill-column, scrolling, line numbers, ...) are in custom.el.

;;; Packages for the commands bound below

(use-package avy :defer t)
(use-package expand-region :defer t)
(use-package move-text :defer t)
(use-package fzf :defer t)
(use-package ag :defer t)

;; Emacs's own regexp searches (C-x p g, dired-do-find-regexp) use ag.
;; Emacs only ships grep/ripgrep/ugrep templates; ag ignores --null in
;; match output, but xref parses its file:line: format.  So file names
;; containing ':' are misread: skipped, or "Search failed" if first.
(with-eval-after-load 'xref
  (add-to-list 'xref-search-program-alist
               ;; --silent: an unreadable file (e.g. a tracked file deleted but not
               ;; staged) would otherwise put an error line first in the output,
               ;; which xref reports as a failed search.
               '(ag . "xargs -0 ag <C> --silent --nogroup --nocolor --filename --numbers -- <R>"))
  (setq xref-search-program 'ag))
(use-package ucs-utils :defer t)

(use-package iedit
  :defer t
  :config (keymap-set iedit-mode-keymap "RET" #'iedit-mode))

;;; Key bindings

(keymap-global-set "C-<down>" #'gcm-scroll-down)
(keymap-global-set "C-<up>"   #'gcm-scroll-up)

(keymap-global-set "<f2>" #'call-last-kbd-macro)
(keymap-global-set "M-<f2>" #'apply-macro-to-region-lines)
(keymap-global-set "C-h n" #'man)
(keymap-global-set "C-x $" #'selective-display)
(with-eval-after-load 'tab-line
  (keymap-unset tab-line-mode-map "C-x <right>")
  (keymap-unset tab-line-mode-map "C-x <left>"))
(keymap-global-set "<f9>" #'recompile)
(keymap-global-set "M-<f9>" #'switch-to-compilation)
(keymap-global-set "C-j" #'join-line-backward)
(keymap-global-set "M-S-<up>" #'move-text-up)
(keymap-global-set "M-S-<down>" #'move-text-down)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-=" #'er/expand-region)
(keymap-global-set "M-+" #'er/contract-region)
(keymap-global-set "M-B" #'bring-sexp)
(keymap-global-set "M-r" #'repeat)
(keymap-global-set "M-j" #'avy-goto-word-1)
(keymap-global-set "C-o" #'open-line-better)
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-<delete>" #'kill-word)
(keymap-global-set "C-x n i" #'narrow-to-region-indirect)
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(keymap-global-set "M-s-<right>" #'next-buffer)
(keymap-global-set "M-s-<left>" #'previous-buffer)


(defvar-keymap fzf-map
  "f" #'fzf
  "d" #'fzf-find-file-in-dir
  "b" #'fzf-switch-buffer)

(defvar-keymap personal-map
  "w" #'agent-shell-send-dwim
  "d" #'duplicate-current-line-or-region
  "C-p" #'compile
  "a" #'magit-status
  "b b" #'magit-blame
  "o" #'occur
  "'" #'iedit-mode                      ; Ctrl-c '
  "0" #'insert-console-log
  "i" #'iedit-mode
  "j" #'json-pretty-print
  "r" (lambda () (interactive) (revert-buffer t t t))
  "f g" #'lgrep
  "C-q" #'fill-region
  "q" #'unfill
  "m b" #'kmacro-bind-to-key
  "m d" #'global-unset-key
  "f f" #'find-file-at-point
  "f i" #'insert-file-name
  "f b" #'browse-url-at-point
  "f d" #'find-name-dired
  "f a" #'ag
  "t" #'auto-revert-tail-mode
  "v" #'view-mode
  "y" (lambda () (interactive) (flyspell-mode) (flyspell-buffer))
  "<up>" #'up-list-backward-sexp
  "<down>" #'up-list-pop
  "-" #'text-scale-decrease
  "=" #'text-scale-increase
  "TAB" #'set-tab-width                 ; TAB, not <tab>: works in a terminal too
  "h" #'highlight-phrase
  "C-h" #'highlight-regexp
  "c" #'calculator
  "n" #'flycheck-next-error
  "," #'lsp-find-references
  "f z" fzf-map
  "f j" #'jump-to-code)
(keymap-global-set "C-c" personal-map)

                                        ;(define-key ghostel-mode-map "l", 'ghostel-clear)

(defun indent-riffle (arg)
  "Indent the current line and move to the next one, ARG times."
  (interactive "p")
  (dotimes (_ arg)
    (indent-for-tab-command)
    (forward-line 1)))
(when window-system (keymap-global-set "C-z" #'indent-riffle))

(defun isearch-forward-symbol-at-point-now ()
  "Search forward for the next occurrence of the symbol at point, wrapping."
  (interactive)
  (isearch-forward-symbol-at-point)
  (let ((isearch-wrap-pause 'no-ding))
    (isearch-repeat-forward)))

(keymap-global-set "M-'" #'isearch-forward-symbol-at-point-now)

(defun unmark-sexp ()
  "Undo the last `mark-sexp' extension."
  (interactive)
  (mark-sexp (if (> (mark) (point)) -1 1) t))
(keymap-global-set "C-M-<return>" #'unmark-sexp)
