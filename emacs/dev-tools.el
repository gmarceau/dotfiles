;;; dev-tools.el --- Compilation, formatting and syntax checking  -*- lexical-binding: t -*-

;;; Compilation

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

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun switch-to-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

;; Python binds C-c C-p to run-python; other modes reach the global
;; C-c C-p in personal-map.
(with-eval-after-load 'python
  (keymap-set python-base-mode-map "C-c C-p" #'compile))

;;; Formatting

;; Apheleia and treesit setup from https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;;
;; Format on save only where the project uses that formatter, so editing
;; one line in someone else's repo doesn't reformat the whole file.
;; Elsewhere ws-butler (core.el) just trims the lines I touched.
;; `M-x apheleia-format-buffer' still works anywhere.

(defconst my/prettier-config-files
  '(".prettierrc" ".prettierrc.json" ".prettierrc.json5" ".prettierrc.yaml"
    ".prettierrc.yml" ".prettierrc.toml" ".prettierrc.js" ".prettierrc.cjs"
    ".prettierrc.mjs" ".prettierrc.ts" ".prettierrc.cts" ".prettierrc.mts"
    "prettier.config.js" "prettier.config.cjs" "prettier.config.mjs"
    "prettier.config.ts" "prettier.config.cts" "prettier.config.mts")
  "Files whose presence means a project uses Prettier.")

(defun my/file-above-matches-p (name regexp)
  "Non-nil if a file NAME in this or a parent directory contains REGEXP."
  (let ((start (or buffer-file-name default-directory)))
    (locate-dominating-file
     start
     (lambda (dir)
       (let ((file (expand-file-name name dir)))
         (and (file-readable-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (re-search-forward regexp nil t))))))))

(defun my/file-above-p (names)
  "Non-nil if any of NAMES exists in this or a parent directory."
  (let ((start (or buffer-file-name default-directory)))
    (seq-some (lambda (name) (locate-dominating-file start name)) names)))

(defun my/project-uses-formatter-p (formatter)
  "Non-nil if the current buffer's project is set up for FORMATTER."
  (let ((name (symbol-name formatter)))
    (cond
     ((string-prefix-p "prettier" name)
      (or (my/file-above-p my/prettier-config-files)
          ;; A "prettier" key or dependency in package.json.
          (my/file-above-matches-p "package.json" "\"prettier\"[[:space:]]*:")))
     ((eq formatter 'black)
      (my/file-above-matches-p "pyproject.toml" "^\\[tool\\.black\\]"))
     ((eq formatter 'isort)
      (or (my/file-above-p '(".isort.cfg"))
          (my/file-above-matches-p "pyproject.toml" "^\\[tool\\.isort\\]")))
     ((string-prefix-p "ruff" name)
      (or (my/file-above-p '("ruff.toml" ".ruff.toml"))
          (my/file-above-matches-p "pyproject.toml" "^\\[tool\\.ruff")))
     ((eq formatter 'clang-format)
      (my/file-above-p '(".clang-format" "_clang-format")))
     ;; These are the standard formatters for their languages, so every
     ;; project in them is assumed to use them.  lisp-indent is Emacs's
     ;; own indentation, which Emacs Lisp projects follow.
     ((memq formatter '(gofmt goimports gofumpt rustfmt terraform lisp-indent))
      t))))

(defun my/apheleia-no-project-formatter-p ()
  "For `apheleia-inhibit-functions': non-nil unless the project uses
every formatter Apheleia would run on this buffer."
  (let ((formatters (and (fboundp 'apheleia--get-formatters)
                         (apheleia--get-formatters))))
    (not (and formatters
              (seq-every-p #'my/project-uses-formatter-p formatters)))))

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
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform)
  (add-hook 'apheleia-inhibit-functions #'my/apheleia-no-project-formatter-p)
  (apheleia-global-mode +1))

(use-package prettier :defer t)

;;; Checking

(use-package flycheck :commands flycheck-next-error)
(use-package coverlay :defer t)          ; M-x coverlay-watch-file on an lcov file

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;;               ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;;               ("M-p" . flycheck-previous-error)))

;;; Version control

(use-package magit :defer t)
(use-package difftastic :defer t)
(use-package lab :defer t)
