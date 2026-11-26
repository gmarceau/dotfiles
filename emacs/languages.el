;;; languages.el --- File types and per-language configuration  -*- lexical-binding: t -*-

;;; File types

(add-to-list 'auto-mode-alist '("\\.hjson\\'" . js-mode) t)
(add-to-list 'auto-mode-alist '("\\.axn\\'" . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode) t)
(add-to-list 'auto-mode-alist '("log\\'" . auto-revert-tail-mode) t)
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode) t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode) t)

;;; Python

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

(eval-after-load "js"
  '(define-key js-mode-map "\C-c>" 'insert-js-result-marker))
(eval-after-load "typescript-ts-mode"
  '(define-key typescript-ts-base-mode-map "\C-c>" 'insert-js-result-marker))

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

(defun nvm-which ()
  (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
    (cadr (split-string output "[\n]+" t))))
(setq nodejs-repl-command #'nvm-which)

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

;;; Markdown

(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c-" 'text-scale-decrease)))
