(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setenv "PATH"
	(concat
	 ":/Users/gmarceau/bin"
	 ":/Users/gmarceau/.local/bin"
	 ":/usr/local/bin"
	 ":"
	 (getenv "PATH")))

(add-to-list 'load-path "~/dotfiles/lisp")

(package-initialize)
(require 'save-packages)
(save-packages)

(add-to-list 'auto-mode-alist '("\\.hjson\\'" . js-mode) t)
(add-to-list 'auto-mode-alist '("\\.axn\\'" . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode) t)

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
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
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

(defun visit-project-tags ()
  (interactive)
  (require 'full-ack)
  (let ((tags-file (concat (ack-guess-project-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun build-project-tags ()
  (interactive)
  (require 'full-ack)
  (message "Building project tags")
  (let* ((tags-table-list '())
         (root (ack-guess-project-root))
         (cmd "ctags -eR --python-kinds=-iv --exclude=.idea --exclude=node_modules --exclude=.git --exclude=lib -o ")
         (tag-file (concat root "TAGS")))
    (shell-command (concat cmd tag-file " " root)))
  (visit-project-tags))

(defun find-tag-at-point ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))

(defun json-reformat-region ()
  "Send the region through `python -m json.tool`"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python -m json.tool" t t))

;; (defun insert-console-log ()
;;   (interactive)
;;   (beginning-of-line)
;;   (open-line 1)
;;   (insert (format "console.log('--%d', );" (line-number-at-pos)))
;;   (insert (format "print '--%d', " (line-number-at-pos)))
;;   (backward-char 2)
;;   (indent-for-tab-command))

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
(global-set-key "\C-j" (lambda () (interactive) (join-line -1)))
(global-set-key "\C-x$" 'selective-display)
(global-set-key [f3] 'next-buffer)
(global-set-key [S-f3] 'previous-buffer)
(global-set-key [M-S-f3] 'bury-buffer)
(global-set-key [f4] 'projectile-ag)
(global-set-key [M-f6] 'iedit-mode)
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
(defvar personal-map (make-sparse-keymap))
(define-key global-map [(control c)] personal-map)

(define-key personal-map "d" 'duplicate-current-line-or-region)
(define-key personal-map "\C-p" 'compile)
(define-key personal-map "p" 'projectile-command-map)
(define-key personal-map "a" 'magit-status)
(define-key personal-map "o" 'occur)
(define-key personal-map "'" 'iedit-mode) ; Ctrl-c '
;; (define-key personal-map "0" 'insert-console-log)
(define-key personal-map "i" 'iedit-mode)
(define-key personal-map "j" 'json-reformat-region)
(define-key personal-map "r" (lambda () (interactive) (revert-buffer t t t)))
(define-key personal-map "a" 'magit-status)
(define-key personal-map "fg" 'lgrep)
(define-key personal-map "\C-q" 'fill-region)
(define-key personal-map "q" 'unfill)
(define-key personal-map "mb" 'bind-last-kdb-macro)
(define-key personal-map "md" 'global-unset-key)
(define-key personal-map "l" 'font-lock-fontify-buffer)
(define-key personal-map "ff" 'find-file-at-point)
(define-key personal-map "fi" 'insert-file-name)
(define-key personal-map "t" 'auto-revert-tail-mode)
(define-key personal-map "v" 'view-mode)
(define-key personal-map "y" (lambda () (interactive) (flyspell-mode) (flyspell-buffer)))
(define-key personal-map "." 'visit-tags-table)
(define-key personal-map [up] 'up-list-backward-sexp)
(define-key personal-map [down] 'up-list-pop)
(define-key personal-map "-" 'text-scale-decrease)
(define-key personal-map "=" 'text-scale-increase)
(define-key personal-map [tab] 'set-tab-width)
(define-key personal-map "h" 'highlight-phrase)
(define-key personal-map "\C-h" 'highlight-regexp)
(define-key personal-map "fd" 'find-name-dired)
(define-key personal-map "fa" 'ag)
(define-key personal-map "c" 'calculator)

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

(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(setq yas-snippet-dirs '("~/dotfiles/snippets"))
(yas-global-mode 1)
(show-paren-mode 1)


(setq shell-command-switch "-ic")
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
(setq ring-bell-function (lambda () ))
(column-number-mode t)
(global-linum-mode t)
(setq backup-directory-alist `(("." . "~/.emacs.d/autosaves")))

(require 'compile)

                                        ;(add-to-list
                                        ; 'compilation-error-regexp-alist
                                        ; '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
                                        ;   1 2 nil (3 . 4)))

;; Add NodeJS error format
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                    1 ;; file
                    2 ;; line
                    3 ;; column
                    ))
(add-to-list 'compilation-error-regexp-alist 'node)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-context 1)
 '(ack-prompt-for-directory t)
 '(auto-revert-interval 0.3)
 '(autopair-blink t)
 '(autopair-blink-delay 0.05)
 '(autopair-global-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output (quote first-error))
 '(compile-command "tg plan")
 '(create-lockfiles nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "70b51a849b665f50a97a028c44cec36b398398357d8f7c19d558fe832b91980f" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy jade javascript-eslint javascript-jshint javascript-gjslint javascript-jscs javascript-standard json-jsonlint json-python-json less luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing puppet-parser puppet-lint python-pylint python-pycompile r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-display-errors-delay 0.3)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-auto-merge-work-directories-length -1)
 '(indent-tabs-mode nil)
 '(initial-scratch-message "")
 '(ispell-program-name "/usr/local/bin/ispell")
 '(js-indent-level 2)
 '(magit-git-global-arguments
   (quote
    ("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c" "log.showSignature=false" "-c" "status.showUntrackedFiles=no")))
 '(magit-section-initial-visibility-alist (quote ((stashes . hide))))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (groovy-mode terraform-mode bazel-mode csproj-mode yasnippet yaml-mode ucs-utils tern swiper sr-speedbar solarized-theme smartrep request-deferred realgud pyvenv python-environment projectile php-mode paredit nav-flash move-text monokai-theme mocha markdown-mode magit-gh-pulls json-mode iedit highlight-indentation google-this full-ack flycheck flx-ido find-file-in-project fill-column-indicator expand-region exec-path-from-shell elmacro ein d-mode cython-mode csharp-mode company-math company-anaconda cmake-mode autopair ag ace-jump-mode)))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#eee8d5")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(projectile-other-file-alist
   (quote
    (("spec.js" "js" "impl.js")
     ("impl.js" "spec.js" "js")
     ("js" "impl.js" "spec.js")
     ("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "hh" "hpp")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" ""))))
 '(realgud-safe-mode nil)
 '(repository-root-matchers (quote (repository-root-matcher/git)))
 '(safe-local-eval-forms (quote ((text-scale-mode t))))
 '(safe-local-variable-values
   (quote
    ((text-scale-mode-amount . 2)
     (text-scale-mode-amount . 3))))
 '(save-packages-file "~/dotfiles/save-packages")
 '(scroll-margin 0)
 '(server-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(split-width-threshold 260)
 '(sr-speedbar-right-side nil)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-also-auto-indent-first-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Aurulent Sans Mono"))))
 '(ack-match ((t (:background "orange" :foreground "black")))))

(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-word 'disabled nil)
(put 'edit-kbd-macro 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(projectile-mode t)
;(projectile-register-project-type 'npm '("package.json") "npm run lint" "npm run test" "npm start")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(remove-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook
 'after-save-hook
 (lambda ()
   (when (string-match
          "[.]spread[.]js$"
          (buffer-file-name))
     (shell-command (concat "spread.js " (buffer-file-name) " &")))))


(add-hook 'find-file-hook 'package-json-hook)
(defun package-json-hook ()
  (when (string= (file-name-nondirectory (buffer-file-name)) "package.json")
    (setq-local js-indent-level 2)))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c-" 'text-scale-decrease)))

(eval-after-load "python"
  '(define-key python-mode-map "\C-c\C-p" 'compile))

(eval-after-load "js"
  '(define-key js-mode-map
     "\C-c>"
     (lambda () (interactive)
       (progn
         (end-of-line)
         (insert " //-> 0 ")))))


(eval-after-load "json-mode"
  '(define-key json-mode-map "\C-c\C-p" 'compile))

(eval-after-load "c++-mode"
  '(define-key c++-mode-map "\C-c\C-p" 'compile))

(eval-after-load "iedit"
  '(progn
     (define-key iedit-mode-keymap "\r" 'iedit-mode)))
