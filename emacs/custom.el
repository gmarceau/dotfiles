;;; custom.el --- Customize output, see `custom-file' in core.el  -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-context 1)
 '(ack-prompt-for-directory t)
 '(ag-ignore-list '("node_modules"))
 '(apheleia-mode-alist
   '((asm-mode . asmfmt) (awk-mode . gawk) (bash-ts-mode . shfmt)
     (bazel-mode . buildifier) (beancount-mode . bean-format)
     (c++-ts-mode . clang-format) (caddyfile-mode . caddyfmt)
     (cc-mode . clang-format) (c-mode . clang-format) (c-ts-mode . clang-format)
     (c++-mode . clang-format) (caml-mode . ocamlformat) (clojure-mode . cljfmt)
     (clojure-ts-mode . cljfmt) (cmake-mode . cmake-format)
     (cmake-ts-mode . cmake-format) (common-lisp-mode . lisp-indent)
     (conf-toml-mode . dprint) (cperl-mode . perltidy)
     (crystal-mode . crystal-tool-format) (css-mode . prettier-css)
     (css-ts-mode . prettier-css) (dart-mode . dart-format)
     (dart-ts-mode . dart-format) (dockerfile-mode . dprint)
     (elixir-mode . mix-format) (elixir-ts-mode . mix-format)
     (elm-mode . elm-format) (emacs-lisp-mode . lisp-indent)
     (fish-mode . fish-indent) (gleam-ts-mode . gleam) (go-mode . gofmt)
     (go-ts-mode . gofmt) (graphql-mode . prettier-graphql)
     (haskell-mode . ormolu) (hcl-mode . hclfmt) (html-mode . prettier-html)
     (html-ts-mode . prettier-html) (java-mode . google-java-format)
     (java-ts-mode . google-java-format) (jinja2-mode)
     (js3-mode . prettier-javascript) (js-json-mode . prettier-json)
     (js-mode . prettier-javascript) (js-ts-mode . prettier-javascript)
     (json-mode . prettier-json) (json-ts-mode . prettier-json)
     (kotlin-mode . ktlint) (kotlin-ts-mode . ktlint) (latex-mode . latexindent)
     (LaTeX-mode . latexindent) (lua-mode . stylua) (lua-ts-mode . stylua)
     (lisp-mode . lisp-indent) (nasm-mode . asmfmt) (nix-mode . nixfmt)
     (perl-mode . perltidy) (php-mode . phpcs) (purescript-mode . purs-tidy)
     (python-mode . black) (python-ts-mode . black) (robot-mode . robotidy)
     (ruby-mode . prettier-ruby) (ruby-ts-mode . prettier-ruby)
     (rustic-mode . rustfmt) (rust-mode . rustfmt) (rust-ts-mode . rustfmt)
     (snakemake-mode . snakefmt) (scss-mode . prettier-scss)
     (sql-mode . pgformatter) (svelte-mode . prettier-svelte)
     (terraform-mode . terraform) (TeX-latex-mode . latexindent)
     (TeX-mode . latexindent) (tsx-ts-mode . prettier-typescript)
     (tuareg-mode . ocamlformat) (typescript-mode . prettier-typescript)
     (typescript-ts-mode . prettier-typescript) (typst-mode . typstyle)
     (typst-ts-mode . typstyle) (v-mode . vfmt) (web-mode . prettier)
     (yaml-mode . prettier-yaml) (yaml-ts-mode . prettier-yaml)
     (yang-mode . pyang) (zig-mode . zig-fmt) (zig-ts-mode . zig-fmt)
     (markdown-mode . prettier)))
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
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "70b51a849b665f50a97a028c44cec36b398398357d8f7c19d558fe832b91980f" default))
 '(eca-chat-use-side-window nil)
 '(eca-send-process-id t)
 '(eca-worktree-mode 'isolated)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elpy-syntax-check-command "pylint")
 '(fill-column 80)
 '(flycheck-checkers
   '(ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
              chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd
              emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis
              fortran-gfortran go-gofmt go-golint go-vet go-build go-test
              go-errcheck groovy haml handlebars haskell-stack-ghc haskell-ghc
              haskell-hlint html-tidy jade javascript-eslint javascript-jshint
              javascript-gjslint javascript-jscs javascript-standard
              json-jsonlint json-python-json less luacheck lua perl
              perl-perlcritic php php-phpmd php-phpcs processing puppet-parser
              puppet-lint python-pylint python-pycompile r-lintr racket
              rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint ruby
              ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint
              scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim
              sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator
              xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-display-errors-delay 0)
 '(flycheck-sql-sqlint-executable "/opt/homebrew/bin/sql-lint")
 '(fzf/window-height 15)
 '(global-display-line-numbers-mode t)
 '(gptel-model 'claude-3-sonnet)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map (solarized-color-blend it "#fdf6e3" 0.25)
          '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16"
            "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50)
     ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF"
     "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"
     "#fdf6e3"))
 '(ido-auto-merge-work-directories-length -1)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(ispell-program-name "~/bin/ispell")
 '(js-indent-level 2)
 '(lsp-enable-indentation nil)
 '(magit-diff-use-overlays nil)
 '(magit-git-global-arguments
   '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c"
     "log.showSignature=false" "-c" "status.showUntrackedFiles=no"))
 '(magit-section-initial-visibility-alist '((stashes . hide)))
 '(make-backup-files nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198"
     "#d33682" "#6c71c4"))
 '(ns-command-modifier 'meta)
 '(package-selected-packages
   '(ace-jump-mode ag agent-shell aider apheleia bazel claude-code company corfu
                   coverlay csv-mode difftastic dockerfile-mode eca elmacro elpy
                   exec-path-from-shell expand-region find-file-in-project
                   flx-ido flycheck flymake-shellcheck full-ack fzf ghostel
                   go-mode groovy-mode iedit json-mode lab lsp-mode lsp-ui magit
                   markdown-mode monet monokai-theme move-text multi-term
                   nodejs-repl origami paredit perspective prettier projectile
                   pylint python-environment pyvenv request-deferred rust-mode
                   smartparens smartrep solarized-theme sql-indent swiper
                   terraform-mode tree-sitter tree-sitter-langs typescript-mode
                   ucs-utils xterm-color yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(persp-state-default-file "~/.emacs.perspective")
 '(pixel-scroll-precision-mode t)
 '(pos-tip-background-color "#eee8d5")
 '(projectile-completion-system 'auto)
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn"
     ".stack-work" "node_modules"))
 '(projectile-other-file-alist
   '(("spec.js" "js" "impl.js") ("impl.js" "spec.js" "js")
     ("js" "impl.js" "spec.js") ("cpp" "h" "hpp" "ipp") ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc") ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx") ("hxx" "h" "ixx" "cxx") ("c" "h") ("m" "h")
     ("mm" "h") ("h" "c" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "hh" "hpp") ("hh" "cc") ("vert" "frag") ("frag" "vert")
     (nil "lock" "gpg") ("lock" "") ("gpg" "")))
 '(realgud-safe-mode nil)
 '(repository-root-matchers '(repository-root-matcher/git))
 '(safe-local-eval-forms '((text-scale-mode t)))
 '(safe-local-variable-values
   '((text-scale-mode-amount . 2) (text-scale-mode-amount . 3)))
 '(save-packages-file "~/dotfiles/save-packages")
 '(scroll-conservatively 101)
 '(scroll-margin 5)
 '(server-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(split-width-threshold 260)
 '(sql-mysql-program "~/bin/mysql")
 '(sr-speedbar-right-side nil)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900")
     (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600")
     (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65")
     (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba")
     (340 . "#268fc6") (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900"
                 "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682"
                 "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-also-auto-indent-first-line t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#161819" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Menlo")))))
