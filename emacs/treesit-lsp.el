;;; treesit-lsp.el --- Tree-sitter grammars and modes, LSP  -*- lexical-binding: t -*-

;;; Tree-sitter

;; Emacs 31 modes ship their own grammar recipes, pinned to commits that
;; match their font-lock rules, and offer to install a missing grammar on
;; first use (`treesit-auto-install-grammar').  Only grammars Emacs has no
;; recipe for are listed here.
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))

(use-package treesit
  :ensure nil
  :demand t
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ;; More modes defined here...
         )
  :config
  ;; Visit these languages in their tree-sitter modes.  The option's :set
  ;; function does the remapping; it's called directly because `setopt'
  ;; and `customize-set-variable' load the Customize UI at startup.
  (funcall (get 'treesit-enabled-modes 'custom-set)
           'treesit-enabled-modes
           '(python-ts-mode css-ts-mode typescript-ts-mode c-ts-mode c++-ts-mode
             c-or-c++-ts-mode bash-ts-mode json-ts-mode dockerfile-ts-mode))
  ;; Not in Emacs's remap table: JavaScript is parsed with the TypeScript
  ;; grammar, and json-mode is a MELPA package.
  (add-to-list 'major-mode-remap-alist '(js-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

;;; LSP and completion

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


(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :commands lsp-find-references          ; C-c , before any LSP buffer
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-diagnostics-provider :flycheck)
  (lsp-enable-indentation nil)          ; indent with the major mode
  ;; TypeScript 7 no longer ships tsserver.js, which ts-ls
  ;; (typescript-language-server) wraps; use tsgo, TS 7's native server
  ;; (M-x lsp-install-server RET tsgo).  jsts-ls is the unmaintained
  ;; sourcegraph server, which would otherwise outrank tsgo.
  (lsp-disabled-clients '(ts-ls jsts-ls))
  :config
  ;; tsgo rejects the null fields older lsp-mode sends ("cannot unmarshal
  ;; ... inlineCompletion"); fixed in lsp-mode PR #5113 (Oct 2026).
  ;; Assumes a package.el install: a :vc checkout's version comes from its
  ;; Version: header, so this would warn wrongly there.
  (let ((desc (cadr (assq 'lsp-mode package-alist))))
    (when (and desc (version-list-< (package-desc-version desc) '(20261001)))
      (display-warning
       'dotfiles
       (format "lsp-mode %s is too old for tsgo (TypeScript 7). Fix: M-x package-upgrade RET lsp-mode"
               (package-version-join (package-desc-version desc)))))))

(use-package lsp-ui :defer t)            ; lsp-mode turns it on when installed

;; lsp-mode turns company on in LSP buffers when it's installed.
(use-package company
  :defer t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

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
