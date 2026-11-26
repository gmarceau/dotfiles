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
