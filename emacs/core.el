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
