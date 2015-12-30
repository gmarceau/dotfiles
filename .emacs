(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


(setenv "BODYLABS" "/Users/gmarceau/Documents/core")

(setenv "PYTHONPATH"
	(concat
	 (getenv "BODYLABS")
	 ":/Users/gmarceau/Documents"
	 ":/usr/local/lib/python2.7/site-packages"
	 ":/Library/Python/2.7/site-packages"
	 ":/Users/gmarceau/miniconda/lib/python2.7/site-packages"
	 ":"
	 (getenv "PYTHONPATH")))

(setenv "PATH"
	(concat
	 ":/Users/gmarceau/.local/bin"
	 ":/Users/gmarceau/miniconda/bin"
	 ":/usr/local/bin"
	 ":"
	 (getenv "PATH")))

(package-initialize)
(elpy-enable)
(define-key elpy-mode-map [(control down)] nil)
(define-key elpy-mode-map [(control up)] nil)
(define-key elpy-mode-map "\M-]" 'elpy-goto-definition)
(define-key elpy-mode-map "\M-[" 'pop-tag-mark)
(define-key elpy-mode-map "\C-c\C-e" 'elpy-multiedit)
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 3))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 3))

(defun recompile ()
  (interactive)
  (compile compile-command))

(defun join-line-backward ()
  (interactive)
  (join-line -1)
  (indent-for-tab-command))

(defun switch-to-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

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

(defun count-words ()
  (interactive)
  (save-excursion
    (if (< (mark) (point)) (exchange-point-and-mark))
    (let ((end (mark)))
      (message "%d words"
               (while-break ((i 0)) 
                 (if (not (re-search-forward "\\<" end t)) (break i)
                   (setq i (+ 1 i))
                   (forward-char 1)))))))

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


(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)]   'gcm-scroll-up)

(global-set-key [f2] 'call-last-kbd-macro)
(global-set-key [\M-f2] 'apply-macro-to-region-lines)
(global-set-key "\C-hn" 'man)
(global-set-key "\C-j" '(lambda () (interactive) (join-line -1)))
(global-set-key "\C-x$" 'selective-display)
(global-set-key [f3] 'next-buffer)
(global-set-key [S-f3] 'previous-buffer)
(global-set-key [M-S-f3] 'bury-buffer)
(global-set-key [f9] 'recompile)
(global-set-key [M-f9] 'switch-to-compilation)
(global-set-key "\C-j" 'join-line-backward)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key "\M-o" 'other-window)


(defvar personal-map (make-sparse-keymap))
(define-key global-map [(control c)] personal-map)

(define-key personal-map "d" 'duplicate-current-line-or-region)
(define-key personal-map "p" 'compile)
(define-key personal-map "r" 'revert-buffer)
(define-key personal-map "a" 'magit-status)
(define-key personal-map "o" 'occur)
(define-key personal-map "i" 'set-fill-column)
(define-key personal-map "r" (lambda () (interactive) (revert-buffer t t)))
(define-key personal-map "a" 'magit-status)
(define-key personal-map "g" 'lgrep)
(define-key personal-map "\C-q" 'fill-region)
(define-key personal-map "q" 'unfill)
(define-key personal-map "mb" 'bind-last-kdb-macro)
(define-key personal-map "md" 'global-unset-key)
(define-key personal-map "l" 'font-lock-fontify-buffer)
(define-key personal-map "f" 'find-file-at-point)
(define-key personal-map "t" 'insert-time-stamp)
(define-key personal-map "v" 'view-mode)
(define-key personal-map "y" '(lambda () (interactive) (flyspell-mode) (flyspell-buffer)))
(define-key personal-map "." 'visit-tags-table)
(define-key personal-map [up] 'up-list-backward-sexp)
(define-key personal-map [down] 'up-list-pop)
(define-key personal-map "-" 'text-scale-decrease)
(define-key personal-map "=" 'text-scale-increase)
(define-key personal-map [tab] 'set-tab-width)
(define-key personal-map "h" 'highlight-phrase)
(define-key personal-map "\C-h" 'highlight-regexp)

(fset 'indent-riffle [tab down])
(if window-system (define-key global-map "\C-z" 'indent-riffle))

(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode)
(show-paren-mode 1)


(setq magit-last-seen-setup-instructions "1.4.0")
(delete-selection-mode)

(tool-bar-mode 0)
(blink-cursor-mode 0)
(setq mac-command-modifier 'control)
(setq inhibit-splash-screen t)
(setq visible-bell nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function '(lambda () ))
(column-number-mode t)

(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.5)
 '(autopair-blink t)
 '(autopair-blink-delay 0.05)
 '(autopair-global-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(compile-command "nose2 ")
 '(custom-enabled-themes (quote (tango-dark)))
 '(ispell-program-name "/usr/local/bin/ispell")
 '(sr-speedbar-right-side nil)
 '(traad-server-program (quote ("/Users/gmarceau/miniconda/bin/traad"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-word 'disabled nil)
(put 'edit-kbd-macro 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(add-hook 'python-mode-hook 'eldoc-mode)
;(global-flycheck-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))

;;
