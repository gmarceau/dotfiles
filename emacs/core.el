;;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/dotfiles/lisp")

;; Customize writes here instead of this file
(setq custom-file "~/dotfiles/emacs/custom.el")

;;; Packages
;; Every package is declared with `use-package' next to its config and
;; installed on first use.  `package-selected-packages' is built from
;; those declarations instead of being saved in custom.el, so
;; `package-autoremove' removes whatever is no longer declared.

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defvar declared-packages nil
  "Packages declared with `use-package', for `package-selected-packages'.")

(defun ensure-and-record-package (name args state &optional no-refresh)
  "Record the packages NAME's declaration installs, then install them."
  (dolist (ensure args)
    (let ((package (if (eq ensure t) (use-package-as-symbol name) ensure)))
      (when package
        (push (if (consp package) (car package) package) declared-packages))))
  (use-package-ensure-elpa name args state no-refresh))
(setq use-package-ensure-function #'ensure-and-record-package)

;; :vc declarations skip `use-package-ensure-function'.
(advice-add 'use-package-vc-install :before
            (lambda (arg &rest _) (push (car arg) declared-packages))
            '((name . record-declared-package)))

;; Keep package.el from saving its list to custom.el.
(advice-add 'package--save-selected-packages :override
            (lambda (&optional value)
              (when value (setq package-selected-packages value)))
            '((name . selected-packages-not-in-custom-file)))

(use-package exec-path-from-shell
  :demand t
  ;; PATH is set in .zshrc (via .zsh-env-vars.sh), hence the default -i;
  ;; don't warn about it.
  :custom (exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))


(load "~/dotfiles/emacs/editing.el" nil t)

(ido-mode 1)
(use-package yasnippet
  :demand t
  :init (setq yas-snippet-dirs '("~/dotfiles/snippets"))
  :config (yas-global-mode 1))
(show-paren-mode 1)
(electric-pair-mode 1)

(delete-selection-mode)

(setq inhibit-startup-screen t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)
;; (global-linum-mode t)


(load custom-file nil t)
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


                                        ;(persp-state-load persp-state-default-file)

(use-package perspective
  :defer t
  :custom (persp-suppress-no-prefix-key-warning t)
  :init
  (defun persp-save-state-on-quit ()
    "Save perspectives on quit, turning `persp-mode' on first if needed,
so `persp-state-load' can restore the last session."
    (unless noninteractive
      (unless (bound-and-true-p persp-mode) (persp-mode 1))
      (persp-state-save)))
  (add-hook 'kill-emacs-hook #'persp-save-state-on-quit))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
                                        ;(remove-hook 'before-save-hook 'delete-trailing-whitespace)


                                        ;(define-key perspective-map "s" 'projectile-persp-switch-project)


(load "~/dotfiles/emacs/languages.el" nil t)
(load "~/dotfiles/emacs/dev-tools.el" nil t)
(load "~/dotfiles/emacs/treesit-lsp.el" nil t)
(load "~/dotfiles/emacs/agents.el" nil t)

(dolist (init (file-expand-wildcards "~/dotfiles/*/init.el"))
  (load (file-name-sans-extension init) nil t))

(setq package-selected-packages (sort (delete-dups declared-packages)))
