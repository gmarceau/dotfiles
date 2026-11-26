;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/dotfiles/lisp")

;; Log native-compiler warnings instead of popping up *Warnings*
(setq native-comp-async-report-warnings-errors 'silent)

;; Customize writes here instead of this file
(setq custom-file "~/dotfiles/emacs/custom.el")

(package-initialize)
(exec-path-from-shell-copy-env "PATH")
(require 'save-packages)
                                        ;(require 'tsx-mode)
(save-packages)


(setenv "PATH" (concat (expand-file-name "~/bin:")
                       (expand-file-name "~/.local/bin:")
                       "/usr/local/bin:"
                       (expand-file-name "~/.docker/bin:")
                       (expand-file-name "~/user-python-venv/bin:")
                       (expand-file-name "~/.nvm/versions/node/v18.20.5/bin:")
                       (getenv "PATH")))


(load "~/dotfiles/emacs/editing.el" nil t)

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

(setq company-minimum-prefix-length 1
      company-idle-delay 0.2) ;; default is 0.2


                                        ;(persp-state-load persp-state-default-file)

(add-hook 'kill-emacs-hook #'persp-state-save)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
                                        ;(remove-hook 'before-save-hook 'delete-trailing-whitespace)


                                        ;(define-key perspective-map "s" 'projectile-persp-switch-project)


(load "~/dotfiles/emacs/languages.el" nil t)
(load "~/dotfiles/emacs/dev-tools.el" nil t)
(load "~/dotfiles/emacs/treesit-lsp.el" nil t)
(load "~/dotfiles/emacs/agents.el" nil t)

(dolist (init (file-expand-wildcards "~/dotfiles/*/init.el"))
  (load (file-name-sans-extension init) nil t))
