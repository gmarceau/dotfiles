;;; languages.el --- File types and per-language configuration  -*- lexical-binding: t -*-

;;; File types

(use-package csv-mode :defer t)
(use-package go-mode :defer t)
(use-package json-mode :defer t)
(use-package rust-mode :defer t)
(use-package terraform-mode :defer t)
(use-package yaml-mode :defer t)
(use-package sql-indent :defer t)
(use-package prisma-ts-mode :mode "\\.prisma\\'")
(use-package alloy-mode
  :ensure nil
  :load-path "~/dotfiles/lisp/alloy-mode"
  :mode "\\.als\\'")

(add-to-list 'auto-mode-alist '("\\.hjson\\'" . js-mode) t)
(add-to-list 'auto-mode-alist '("\\.axn\\'" . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode) t)
(add-to-list 'auto-mode-alist '("log\\'" . auto-revert-tail-mode) t)
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode) t)

;;; Python

(use-package pyvenv :defer t)

(add-hook 'python-base-mode-hook 'flycheck-mode)

                                        ;(with-eval-after-load 'flycheck
                                        ;  (put 'python-pylint
                                        ;       (flycheck--checker-property-name 'working-directory)
                                        ;        projectile-project-root
                                        ;       ))

;;; JavaScript and JSON

(defun insert-js-result-marker ()
  "Append \" //-> 0 \" to the current line."
  (interactive)
  (end-of-line)
  (insert " //-> 0 "))

(with-eval-after-load 'js
  (keymap-set js-mode-map "C-c >" #'insert-js-result-marker))
(with-eval-after-load 'typescript-ts-mode
  (keymap-set typescript-ts-base-mode-map "C-c >" #'insert-js-result-marker))

(setopt js-indent-level 2)

(add-hook 'find-file-hook 'package-json-hook)
(defun package-json-hook ()
  (when (string= (file-name-nondirectory (buffer-file-name)) "package.json")
    (setq-local js-indent-level 2)))

(add-hook
 'after-save-hook
 (lambda ()
   (when (string-match
          "[.]spread[.]js$"
          (buffer-file-name))
     (shell-command (concat "spread.js " (buffer-file-name) " &")))))

(defun insert-console-log ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert (format "logger.info('~=%d', );" (line-number-at-pos)))
  (backward-char 2)
  (indent-for-tab-command))


;;; Markdown

(use-package markdown-mode
  :defer t
  :config (keymap-set markdown-mode-map "C-c -" #'text-scale-decrease))
