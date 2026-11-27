;;; custom.el --- Customize output, see `custom-file' in core.el  -*- lexical-binding: t -*-

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
 '(compilation-ask-about-save nil)
 '(compilation-message-face 'default)
 '(compilation-scroll-output 'first-error)
 '(compile-command "tg plan")
 '(confirm-kill-emacs 'y-or-n-p)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(tango-dark))
 '(eca-chat-use-side-window nil)
 '(eca-send-process-id t)
 '(eca-worktree-mode 'isolated)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fill-column 80)
 '(flycheck-display-errors-delay 0)
 '(flycheck-sql-sqlint-executable "/opt/homebrew/bin/sql-lint")
 '(fzf/window-height 15)
 '(global-display-line-numbers-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(ispell-program-name "~/bin/ispell")
 '(js-indent-level 2)
 '(lsp-enable-indentation nil)
 '(magit-git-global-arguments
   '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c"
     "log.showSignature=false" "-c" "status.showUntrackedFiles=no"))
 '(magit-section-initial-visibility-alist '((stashes . hide)))
 '(make-backup-files nil)
 '(ns-command-modifier 'meta)
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(persp-state-default-file "~/.emacs.perspective")
 '(pixel-scroll-precision-mode t)
 '(safe-local-eval-forms '((text-scale-mode t)))
 '(safe-local-variable-values
   '((text-scale-mode-amount . 2) (text-scale-mode-amount . 3)))
 '(scroll-conservatively 101)
 '(scroll-margin 5)
 '(server-mode t)
 '(split-width-threshold 260)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(yas-also-auto-indent-first-line t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#161819" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Menlo")))))
