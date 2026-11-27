;;; agents.el --- AI coding agents: Claude Code, ECA, agent-shell  -*- lexical-binding: t -*-

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
  :bind-keymap ("C-c k" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
(use-package ghostel :defer t)
(setq claude-code-terminal-backend 'ghostel)

(defun osx-claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

(setq claude-code-notification-function #'osx-claude-notify)
(setq claude-code-optimize-window-resize nil)


(use-package eca
  :defer t
  :config

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
  (advice-add 'eca-process-stop :override #'my-eca--graceful-process-stop))

;; Bound at startup: both commands are autoloaded from eca.
(keymap-global-set "S-<f8>" #'eca-chat-go-to-next-attention-in-project)
(keymap-global-set "<f8>" #'eca-chat-go-to-next-attention)

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

  (keymap-set eca-chat-mode-map "C-c C-S-l" #'eca-chat-compact)
  (keymap-set eca-chat-mode-map "C-c C-l" #'eca-chat-clear-with-confirm)
  (keymap-set eca-chat-mode-map "C-c C-!" #'eca-chat-run-shell-command)
  )
(use-package agent-shell :commands agent-shell-send-dwim)
(setq agent-shell-elicitation--experimental-feature-enabled t)

(require 'agent-tools)
