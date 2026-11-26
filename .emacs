;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/dotfiles/lisp")

(package-initialize)
(exec-path-from-shell-copy-env "PATH")
(require 'eca-smolvm)
(require 'save-packages)
                                        ;(require 'tsx-mode)
(save-packages)


(setenv "PATH" (concat "/Users/guillaume/bin:/Users/guillaume/.local/bin:/usr/local/bin:"
                       "/Users/guillaume/.docker/bin:"
                       "/Users/guillaume/user-python-venv/bin:"
                       "/Users/guillaume/.nvm/versions/node/v18.20.5/bin:"
                       (getenv "PATH")))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (sql "https://github.com/DerekStride/tree-sitter-sql")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(add-to-list 'auto-mode-alist '("\\.hjson\\'" . js-mode) t)
(add-to-list 'auto-mode-alist '("\\.axn\\'" . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode) t)
(add-to-list 'auto-mode-alist '("log\\'" . auto-revert-tail-mode) t)
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode) t)

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

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


(defun insert-console-log ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert (format "logger.info('~=%d', );" (line-number-at-pos)))
  (backward-char 2)
  (indent-for-tab-command))

(defun json-reformat-region ()
  "Send the region through `python -m json.tool`"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python -m json.tool" t t))

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

(setq mac-command-modifier 'super)
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

(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(setq yas-snippet-dirs '("~/dotfiles/snippets"))
(yas-global-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(electric-pair-mode 1)

(setq ffip-use-rust-fd t)
(setq shell-command-switch "-ic")
(setq magit-last-seen-setup-instructions "1.4.0")
(delete-selection-mode)

(tool-bar-mode 0)
(blink-cursor-mode 0)
(url-handler-mode)
(setq mac-command-modifier 'control)
(setq inhibit-splash-screen t)
(setq visible-bell nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function (lambda () ))
(column-number-mode t)
;; (global-linum-mode t)
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
 '(ag-ignore-list '("node_modules"))
 '(apheleia-mode-alist
   '((asm-mode . asmfmt) (awk-mode . gawk) (bash-ts-mode . shfmt)
     (bazel-mode . buildifier) (beancount-mode . bean-format)
     (c++-ts-mode . clang-format) (caddyfile-mode . caddyfmt)
     (cc-mode . clang-format) (c-mode . clang-format) (c-ts-mode . clang-format)
     (c++-mode . clang-format) (caml-mode . ocamlformat) (clojure-mode . cljfmt)
     (clojure-ts-mode . cljfmt) (cmake-mode . cmake-format)
     (cmake-ts-mode . cmake-format) (common-lisp-mode . lisp-indent)
     (conf-toml-mode . dprint) (cperl-mode . perltidy)
     (crystal-mode . crystal-tool-format) (css-mode . prettier-css)
     (css-ts-mode . prettier-css) (dart-mode . dart-format)
     (dart-ts-mode . dart-format) (dockerfile-mode . dprint)
     (elixir-mode . mix-format) (elixir-ts-mode . mix-format)
     (elm-mode . elm-format) (emacs-lisp-mode . lisp-indent)
     (fish-mode . fish-indent) (gleam-ts-mode . gleam) (go-mode . gofmt)
     (go-ts-mode . gofmt) (graphql-mode . prettier-graphql)
     (haskell-mode . ormolu) (hcl-mode . hclfmt) (html-mode . prettier-html)
     (html-ts-mode . prettier-html) (java-mode . google-java-format)
     (java-ts-mode . google-java-format) (jinja2-mode)
     (js3-mode . prettier-javascript) (js-json-mode . prettier-json)
     (js-mode . prettier-javascript) (js-ts-mode . prettier-javascript)
     (json-mode . prettier-json) (json-ts-mode . prettier-json)
     (kotlin-mode . ktlint) (kotlin-ts-mode . ktlint) (latex-mode . latexindent)
     (LaTeX-mode . latexindent) (lua-mode . stylua) (lua-ts-mode . stylua)
     (lisp-mode . lisp-indent) (nasm-mode . asmfmt) (nix-mode . nixfmt)
     (perl-mode . perltidy) (php-mode . phpcs) (purescript-mode . purs-tidy)
     (python-mode . black) (python-ts-mode . black) (robot-mode . robotidy)
     (ruby-mode . prettier-ruby) (ruby-ts-mode . prettier-ruby)
     (rustic-mode . rustfmt) (rust-mode . rustfmt) (rust-ts-mode . rustfmt)
     (snakemake-mode . snakefmt) (scss-mode . prettier-scss)
     (sql-mode . pgformatter) (svelte-mode . prettier-svelte)
     (terraform-mode . terraform) (TeX-latex-mode . latexindent)
     (TeX-mode . latexindent) (tsx-ts-mode . prettier-typescript)
     (tuareg-mode . ocamlformat) (typescript-mode . prettier-typescript)
     (typescript-ts-mode . prettier-typescript) (typst-mode . typstyle)
     (typst-ts-mode . typstyle) (v-mode . vfmt) (web-mode . prettier)
     (yaml-mode . prettier-yaml) (yaml-ts-mode . prettier-yaml)
     (yang-mode . pyang) (zig-mode . zig-fmt) (zig-ts-mode . zig-fmt)
     (markdown-mode . prettier)))
 '(auto-revert-interval 0.3)
 '(auto-save-default nil)
 '(autopair-blink t)
 '(autopair-blink-delay 0.05)
 '(autopair-global-mode t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-message-face 'default)
 '(compilation-scroll-output 'first-error)
 '(compile-command "tg plan")
 '(confirm-kill-emacs 'y-or-n-p)
 '(create-lockfiles nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "70b51a849b665f50a97a028c44cec36b398398357d8f7c19d558fe832b91980f" default))
 '(eca-chat-use-side-window nil)
 '(eca-custom-command '("~/code/eca-smolvm/bin/eca-smolvm"))
 '(eca-send-process-id t)
 '(eca-worktree-mode 'isolated)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elpy-syntax-check-command "pylint")
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(flycheck-checkers
   '(ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
              chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd
              emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis
              fortran-gfortran go-gofmt go-golint go-vet go-build go-test
              go-errcheck groovy haml handlebars haskell-stack-ghc haskell-ghc
              haskell-hlint html-tidy jade javascript-eslint javascript-jshint
              javascript-gjslint javascript-jscs javascript-standard
              json-jsonlint json-python-json less luacheck lua perl
              perl-perlcritic php php-phpmd php-phpcs processing puppet-parser
              puppet-lint python-pylint python-pycompile r-lintr racket
              rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint ruby
              ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint
              scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim
              sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator
              xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-display-errors-delay 0)
 '(flycheck-sql-sqlint-executable "/opt/homebrew/bin/sql-lint")
 '(fzf/window-height 15)
 '(global-display-line-numbers-mode t)
 '(gptel-model 'claude-3-sonnet)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map (solarized-color-blend it "#fdf6e3" 0.25)
          '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16"
            "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50)
     ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF"
     "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"
     "#fdf6e3"))
 '(ido-auto-merge-work-directories-length -1)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(ispell-program-name "~/bin/ispell")
 '(js-indent-level 2)
 '(lsp-enable-indentation nil)
 '(magit-diff-use-overlays nil)
 '(magit-git-global-arguments
   '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c"
     "log.showSignature=false" "-c" "status.showUntrackedFiles=no"))
 '(magit-section-initial-visibility-alist '((stashes . hide)))
 '(make-backup-files nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198"
     "#d33682" "#6c71c4"))
 '(ns-command-modifier 'meta)
 '(package-selected-packages
   '(ace-jump-mode ag agent-shell aider apheleia autopair bazel-mode claude-code
                   company corfu coverlay csv-mode difftastic dockerfile-mode
                   eca elmacro elpy exec-path-from-shell expand-region
                   fill-column-indicator find-file-in-project flx-ido flycheck
                   flymake-shellcheck full-ack fzf ghostel go-mode groovy-mode
                   iedit json-mode lab lsp-mode lsp-ui magit markdown-mode monet
                   monokai-theme move-text multi-term nodejs-repl origami
                   paredit perspective prettier projectile pylint
                   python-environment pyvenv request-deferred rust-mode
                   smartparens smartrep smooth-scrolling solarized-theme
                   sql-indent swiper terraform-mode tree-sitter
                   tree-sitter-langs typescript-mode ucs-utils which-key
                   xterm-color yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(persp-state-default-file "~/.emacs.perspective")
 '(pos-tip-background-color "#eee8d5")
 '(projectile-completion-system 'auto)
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn"
     ".stack-work" "node_modules"))
 '(projectile-other-file-alist
   '(("spec.js" "js" "impl.js") ("impl.js" "spec.js" "js")
     ("js" "impl.js" "spec.js") ("cpp" "h" "hpp" "ipp") ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc") ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx") ("hxx" "h" "ixx" "cxx") ("c" "h") ("m" "h")
     ("mm" "h") ("h" "c" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "hh" "hpp") ("hh" "cc") ("vert" "frag") ("frag" "vert")
     (nil "lock" "gpg") ("lock" "") ("gpg" "")))
 '(realgud-safe-mode nil)
 '(repository-root-matchers '(repository-root-matcher/git))
 '(safe-local-eval-forms '((text-scale-mode t)))
 '(safe-local-variable-values
   '((text-scale-mode-amount . 2) (text-scale-mode-amount . 3)))
 '(save-packages-file "~/dotfiles/save-packages")
 '(scroll-margin 3)
 '(server-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smooth-scroll-margin 5)
 '(smooth-scrolling-mode t)
 '(split-width-threshold 260)
 '(sql-mysql-program "~/bin/mysql")
 '(sr-speedbar-right-side nil)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900")
     (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600")
     (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65")
     (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba")
     (340 . "#268fc6") (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900"
                 "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682"
                 "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-also-auto-indent-first-line t))
                                        ;(custom-set-faces
                                        ; ;; custom-set-faces was added by Custom.
                                        ; ;; If you edit it by hand, you could mess it up, so be careful.
                                        ; ;; Your init file should contain only one such instance.
                                        ; ;; If there is more than one, they won't work right.
                                        ; '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Aurulent Sans Mono"))))
                                        ; '(ack-match ((t (:background "orange" :foreground "black")))))

(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-word 'disabled nil)
(put 'edit-kbd-macro 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

                                        ;(projectile-mode t)
                                        ;(projectile-register-project-type 'npm '("package.json") "npm run lint" "npm run test" "npm start")

                                        ;(when (memq window-system '(mac ns))
                                        ;  (exec-path-from-shell-initialize))

(setq shell-command-switch "-c") ;; removing "-i" otherwise zsh complain that the terminal is not interactive

(setq company-minimum-prefix-length 1
      company-idle-delay 0.2) ;; default is 0.2

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

                                        ;(persp-state-load persp-state-default-file)

(add-hook 'kill-emacs-hook #'persp-state-save)

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

(add-hook 'python-mode-hook 'flycheck-mode)

                                        ;(with-eval-after-load 'flycheck
                                        ;  (put 'python-pylint
                                        ;       (flycheck--checker-property-name 'working-directory)
                                        ;        projectile-project-root
                                        ;       ))


                                        ;(define-key perspective-map "s" 'projectile-persp-switch-project)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#161819" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Menlo")))))


(defun nvm-which ()
  (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
    (cadr (split-string output "[\n]+" t))))
(setq nodejs-repl-command #'nvm-which)

;; Apheleia and treesit setup from https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;;
;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))


(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))



(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
(setq claude-code-terminal-backend 'ghostel)

(defun osx-claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

(setq claude-code-notification-function #'osx-claude-notify)
(setq claude-code-optimize-window-resize nil)

;;   ;;;; Code Completion
;; (use-package corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
;;   (corfu-auto-delay 0)            ; No delay for completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;;               ("M-SPC"      . corfu-insert-separator)
;;               ("TAB"        . corfu-next)
;;               ([tab]        . corfu-next)
;;               ("S-TAB"      . corfu-previous)
;;               ([backtab]    . corfu-previous)
;;               ("S-<return>" . corfu-insert)
;;               ("RET"        . corfu-insert))

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))
;;             nil
;;             t))




;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;;               ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;;               ("M-p" . flycheck-previous-error)))




(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-diagnostics-provider :flycheck)
  )

;; (add-to-list 'eglot-server-programs
;;              `(typescript-ts-mode . ("typescript-language-server" "--stdio"
;;                                      :initializationOptions
;;                                      (:maxTsServerMemory 10240))))

;; (use-package lsp-mode
;;   :diminish "LSP"
;;   :ensure t
;;   :hook ((lsp-mode . lsp-diagnostics-mode)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          ((tsx-ts-mode
;;            typescript-ts-mode
;;            js-ts-mode) . lsp-deferred))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
;;   (lsp-completion-provider :none)       ; Using Corfu as the provider
;;   (lsp-diagnostics-provider :flycheck)
;;   (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;   (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
;;   (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
;;   (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
;;   ;; core
;;   (lsp-enable-xref t)                   ; Use xref to find references
;;   (lsp-auto-configure t)                ; Used to decide between current active servers
;;   (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
;;   (lsp-enable-dap-auto-configure t)     ; Debug support
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)              ; I disable folding since I use origami
;;   (lsp-enable-imenu t)
;;   (lsp-enable-indentation nil)          ; I use prettier
;;   (lsp-enable-links nil)                ; No need since we have `browse-url'
;;   (lsp-enable-on-type-formatting nil)   ; Prettier handles this
;;   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
;;   (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
;;   (lsp-enable-text-document-color nil)   ; This is Treesitter's job

;;   (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
;;   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
;;   ;; completion
;;   (lsp-completion-enable t)
;;   (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
;;   (lsp-enable-snippet t)                         ; Important to provide full JSX completion
;;   (lsp-completion-show-kind t)                   ; Optional
;;   ;; headerline
;;   (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; modeline
;;   (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
;;   (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
;;   (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
;;   (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
;;   (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
;;   (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
;;   ;; lens
;;   (lsp-lens-enable nil)                 ; Optional, I don't need it
;;   ;; semantic
;;   (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

;;   :init
;;   (setq lsp-use-plists t))

;; (use-package lsp-completion
;;   :no-require
;;   :hook ((lsp-mode . lsp-completion-mode)))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d" . 'lsp-ui-doc-glance))
;;   :after (lsp-mode evil)
;;   :config (setq lsp-ui-doc-enable t
;;                 evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;;                 lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                 lsp-ui-doc-include-signature t       ; Show signature
;;                 lsp-ui-doc-position 'at-point))



;; init.el
    ;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(with-eval-after-load 'eca

  (defun my-eca--graceful-process-stop (session)
    "Stop SESSION's process without defaulting to SIGKILL."
    (when session
      (let ((proc (ignore-errors (eca--session-process session))))
        (when (and proc (process-live-p proc))
          ;; 1) give it a moment to exit after shutdown/exit
          (let ((deadline (+ (float-time) 0.5)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          ;; 2) SIGTERM
          (when (process-live-p proc)
            (ignore-errors (terminate-process proc))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (process-live-p proc) (< (float-time) deadline))
                (accept-process-output proc 0.05))))
          ;; 3) SIGKILL fallback
          (when (process-live-p proc)
            (ignore-errors (kill-process proc)))))))

  ;; Override only the process-kill step.
  (advice-add 'eca-process-stop :override #'my-eca--graceful-process-stop)
  (global-set-key [S-f8] 'eca-chat-go-to-next-attention-in-project)
  (global-set-key [f8] 'eca-chat-go-to-next-attention))

(with-eval-after-load 'eca-chat
  (defun eca-chat-run-shell-command (command)
    "Run COMMAND and send both it and its output to the current agent."
    (interactive (list (read-shell-command "Shell command: ")))
    (when (string-empty-p (string-trim command))
      (user-error "Shell command cannot be empty"))
    (let ((session (eca-session)))
      (eca-assert-session-running session)
      (when eca-chat--chat-loading
        (user-error "Chat is busy, wait or stop the running prompt"))
      (let* ((output (shell-command-to-string command))
             (indented-command
              (replace-regexp-in-string "^" "    " command))
             (indented-output
              (if (string-empty-p output)
                  "    (no output)"
                (replace-regexp-in-string "^" "    " output))))
        (eca-chat--send-prompt
         session
         (format "Shell command run:\n\n%s\n\nOutput:\n\n%s"
                 indented-command indented-output)))))

  (defun eca-chat-clear-with-confirm ()
    "Clear the current chat buffer, asking for confirmation first."
    (interactive)
    (when (yes-or-no-p "Clear chat and context? ")
      (call-interactively #'eca-chat-clear)))

  (defun eca-chat-compact ()
    "Compact the current chat buffer by sending the /compact command."
    (interactive)
    (let ((session (eca-session)))
      (eca-assert-session-running session)
      (when eca-chat--chat-loading
        (user-error "Chat is busy, wait or stop the running prompt"))
      (let ((extra (string-trim (read-string "Compact prompt (optional): "))))
        (eca-chat--send-prompt session
                               (string-trim (concat "/compact " extra))))))

  (define-key eca-chat-mode-map (kbd "C-c C-S-l") #'eca-chat-compact)
  (define-key eca-chat-mode-map (kbd "C-c C-l") #'eca-chat-clear-with-confirm)
  (define-key eca-chat-mode-map (kbd "C-c C-!") #'eca-chat-run-shell-command)
  )
(setq agent-shell-elicitation--experimental-feature-enabled t)

(defun unmark-sexp ()
  "Undo the last `mark-sexp' extension."
  (interactive)
  (mark-sexp (if (> (mark) (point)) -1 1) t))
(keymap-global-set "C-M-<return>" #'unmark-sexp)

(require 'agent-tools)
(dolist (init (file-expand-wildcards "~/dotfiles/*/init.el"))
  (load (file-name-sans-extension init) nil t))
