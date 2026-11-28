;;; custom.el --- Customize owns the options here: variables, theme, font  -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list '("node_modules"))
 '(auto-revert-interval 0.3)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 1)
 '(compilation-ask-about-save nil nil nil "Save all buffers without asking.")
 '(compilation-message-face 'default)
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'y-or-n-p)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(tango-dark))
 '(dape-buffer-window-arrangement 'right)
 '(dape-inlay-hints t)
 '(eca-chat-use-side-window nil)
 '(eca-send-process-id t)
 '(eca-worktree-mode 'isolated)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(exec-path-from-shell-check-startup-files nil nil nil "PATH is set in .zshrc (via .zsh-env-vars.sh), hence the default -i; don't warn about it.")
 '(fill-column 80)
 '(flycheck-display-errors-delay 0)
 '(flycheck-sql-sqlint-executable "/opt/homebrew/bin/sql-lint")
 '(fzf/window-height 15)
 '(global-display-line-numbers-mode t)
 '(ido-auto-merge-work-directories-length -1 nil nil "Don't search other directories.")
 '(indent-tabs-mode nil)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(ispell-program-name "~/bin/ispell")
 '(js-indent-level 2)
 '(lsp-diagnostics-provider :flycheck)
 '(lsp-disabled-clients '(ts-ls jsts-ls) nil nil "TypeScript 7 no longer ships tsserver.js, which ts-ls (typescript-language-server) wraps; use tsgo, TS 7's native server (M-x lsp-install-server RET tsgo).  jsts-ls is the unmaintained sourcegraph server, which would otherwise outrank tsgo.")
 '(lsp-enable-indentation nil nil nil "Indent with the major mode.")
 '(lsp-keymap-prefix "C-c l")
 '(magit-git-global-arguments
   '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c"
     "log.showSignature=false" "-c" "status.showUntrackedFiles=no")
   nil nil "Faster status in big repos: preload the index, skip signature checks and untracked files.")
 '(magit-section-initial-visibility-alist '((stashes . hide)))
 '(make-backup-files nil)
 '(ns-command-modifier 'meta)
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(persp-state-default-file "~/.emacs.perspective")
 '(persp-suppress-no-prefix-key-warning t)
 '(pixel-scroll-precision-mode t)
 '(safe-local-eval-forms '((text-scale-mode t)))
 '(safe-local-variable-values
   '((text-scale-mode-amount . 2) (text-scale-mode-amount . 3)))
 '(scroll-conservatively 101 nil nil "Scroll a line at a time.  Leave scroll-margin at 0: a margin fights pixel-scroll-precision-mode and snaps the window back during slow scrolls.")
 '(server-mode t)
 '(split-width-threshold 260 nil nil "Split side by side only on very wide frames.")
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(yas-also-auto-indent-first-line t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#161819" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Menlo")))))
