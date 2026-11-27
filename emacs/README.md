# Emacs config

Emacs 31. `~/.emacs` → `.emacs` → `emacs/core.el`; `~/.emacs.d/early-init.el`
→ `emacs/early-init.el` (setup in the top-level `README.md`).

| File | What's in it |
|---|---|
| `early-init.el` | Before packages and the first frame: no toolbar, Menlo 18, package archives, native-comp silent |
| `core.el` | Package machinery, defaults, perspective, loads the files below |
| `editing.el` | Editing/navigation commands, global keys, `C-c` personal map |
| `dev-tools.el` | Compilation, apheleia formatting, flycheck, coverage, magit |
| `languages.el` | File types and per-language settings |
| `treesit-lsp.el` | Tree-sitter grammars and mode remapping, lsp-mode |
| `agents.el` | Claude Code, ECA, agent-shell |
| `custom.el` | Customize output (`custom-file`); don't hand-edit much |

## What changed (Nov 2026 modernization, epic dotfiles-117)

### Keys

| Before | Now |
|---|---|
| `M-]` find tag at point, `M-[` pop tag | **Unbound.** Use `M-.` (`xref-find-definitions`, no prompt for the symbol at point; uses LSP in TS buffers, the Lisp backend in Elisp, TAGS elsewhere) and `M-,` (`xref-go-back`) |
| `C-c >` only in old `js-mode` | Also works in `.js` / `.ts` / `.tsx` buffers (inserts ` //-> 0 ` at end of line) |
| `C-c C-p` in Python ran `run-python` | Runs `compile`, like everywhere else. Start a REPL with `M-x run-python`; `C-c C-z` switches to it |
| `C-z` (GUI) macro: TAB then down | Same, now a command: `C-u 5 C-z` indents 5 lines |
| `C-c l` `font-lock-fontify-buffer` | **Unbound** (font-lock keeps up on its own now). In LSP buffers `C-c l` is the LSP prefix |
| `C-c c` Claude Code prefix (shadowed `calculator`) | `C-c c` is `calculator` again; Claude Code moved to **`C-c k`** |
| `C-c <tab>` set tab width (GUI only) | `C-c TAB`: same command, now also in terminal Emacs |
| `M-'` symbol search | Same behaviour (jumps to next occurrence, wrapping) |
| `M-j` ace-jump-word-mode | `avy-goto-word-1`: same feel (type the word's first char, then the label). Labels are home-row keys; `C-u M-j` limits to the current window (ace-jump ignored `C-u` here). ace-jump-mode was unmaintained since 2013 |

### Behaviour

- **Python buffers have flycheck on** (it was silently off since `.py` opens in
  `python-ts-mode`). `C-c n` jumps to the next error. Flycheck now knows
  all current checkers: ruff, flake8, mypy, pyright, tflint, yamllint,
  hadolint, markdownlint…
- **Killing `*scratch*`** still leaves a fresh, empty one behind (buried).
- **Scrolling**: point keeps 5 lines from the window edge, and the screen
  moves a line at a time (built-in, replaces `smooth-scrolling`).
  Trackpad/mouse scrolling is pixel-smooth (`pixel-scroll-precision-mode`).
  `C-<up>` / `C-<down>` still scroll 3 lines.
- **Compilation colours** are applied only to new output, so long builds stay fast.
- **Command key** is Meta (as before; a dead line saying Control was removed).
- **Quitting saves perspectives.** On quit, `persp-mode` is turned on if
  needed and the window/buffer layout is saved to `~/.emacs.perspective`.
  Restore the last session with `M-x persp-state-load`.
- **`C-x p g` (`project-find-regexp`) and `dired-do-find-regexp` search with ag**
  instead of grep (custom `xref-search-program` entry in `editing.el`).
  Two differences from grep: use `\b` for word boundaries (`\<` and `\>`
  silently match nothing under ag), and files with `:` in their name are
  skipped, or make the search fail ("Search failed") when one of them
  holds the first match.
- **`C-x C-f` no longer opens `http://` URLs** (`url-handler-mode` is off).
- **`.als` files** open in `alloy-mode`; **`.prisma`** files in `prisma-ts-mode`
  (it used to error).
- **Dockerfiles** (`Dockerfile`, `*.Dockerfile`) open in the built-in
  `dockerfile-ts-mode`; the `dockerfile-mode` package is gone.
- **TypeScript/TSX highlighting is complete again**: the grammars were
  rebuilt from Emacs 31's own recipes (the old v0.20 pins didn't match
  Emacs 31's font-lock rules).
- The first frame opens without a toolbar and at its final size (no flash).

### Commands

| Before | Now |
|---|---|
| `M-x debug-on-error`, `M-x truncate-lines` | Same names still work (aliases of `toggle-debug-on-error`, `toggle-truncate-lines`) |
| `C-c f i` with empty input | Now inserts the current buffer's file name, as documented (was broken) |
| `M-x indent-buffer` | Now indents the whole buffer correctly |
| `C-c j` reformatted JSON with `python -m json.tool` (broken: no `python` on PATH) | Built-in `json-pretty-print` on the region (2-space indent). Invalid JSON: truncated input does nothing; trailing junk reformats the valid part, then errors |
| `C-u C-o` errored | Opens a line 4 lines down (`C-u C-u`: 16, `M-- C-o`: 1 up) |
| `C-c m b` ran an undefined `bind-last-kdb-macro` | `kmacro-bind-to-key` (same as `C-x C-k b`): bind the last macro to a key for this session. To keep one, name it (`C-x C-k n`) and `M-x insert-kbd-macro` into your config |
| `M-x ack` (full-ack) | Removed: use `C-c f a` (`ag`) |
| `M-x find-file-in-project` | Removed: use `C-c f z f` (fzf) |
| `C-c p` projectile prefix, `<f4>` projectile-ag | Removed with projectile. `<f4>` is back to the Emacs default (end/call keyboard macro). Use `C-c f z f` (fzf) and `C-c f a` (ag) |
| `M-x swiper` | Removed: use `C-s` / `M-'` |
| `M-x multi-term` | Removed: use ghostel |
| `M-x nodejs-repl`, `M-x pylint`, elpy, aider, paredit, smartparens, origami | Removed |
| `C-c w`, `C-c n`, `<f8>`, `S-<f8>` | Now work right after startup (they used to do nothing until agent-shell / flycheck / ECA had loaded) |
| Other removed packages | groovy-mode, bazel, typescript-mode (typescript-ts-mode is used), tree-sitter + tree-sitter-langs (built-in treesit is used), flymake-shellcheck (flycheck has shellcheck), flx-ido, corfu, xterm-color, smartrep, fill-column-indicator, smooth-scrolling, the ELPA which-key (built in, still off), monokai and solarized themes |
| pycoverage | Removed: `coverage lcov`, then `M-x coverlay-watch-file` on the `.lcov` file (highlights uncovered lines, refreshes when the file changes; works for JS/TS too) |

### Packages

- Every package is declared with `use-package` next to its config, and
  installs itself on first start. **To add a package**, add
  `(use-package NAME :defer t)` (or with `:config …`) to the topic file;
  don't use `M-x package-install` alone, or the next autoremove deletes it.
- **To remove one**, delete its `use-package` form, restart, then
  `M-x package-autoremove`.
- `package-selected-packages` is computed from the declarations at startup
  and is no longer saved in `custom.el`. `save-packages` is gone.
- Built-in libraries are declared with `:ensure nil`.
- Machine-specific config: `~/dotfiles/<name>/init.el`, loaded last.
  Declare its packages with `use-package` too, before running
  `package-autoremove` there. Libraries that aren't on ELPA (a local
  `.el` file) need `:ensure nil`, or every start tries to install them.
- If `~/.emacs.d/early-init.el` is missing, startup shows a `dotfiles`
  warning with the `ln -s` command: without it MELPA isn't configured.

### Tree-sitter

- Emacs 31 modes carry their own grammar recipes, pinned to commits that
  match their highlighting. When a grammar is missing, Emacs **asks to
  install it** the first time you open such a file (needs a C compiler).
  Reinstall one by hand with `M-x treesit-install-language-grammar`.
- The only custom recipe left is prisma (`treesit-lsp.el`). The old
  `os/setup-install-grammars` (installed old pinned grammars at startup)
  is gone.
- Which languages use tree-sitter modes is set by `treesit-enabled-modes`:
  Python, CSS, TypeScript, C/C++, Bash, JSON, Dockerfile. JavaScript (`.js`, `.mjs`,
  `.cjs`, `.jsx`, and `js-mode`) still opens in the TypeScript modes, and
  `json-mode` maps to `json-ts-mode`. YAML, Go, Rust, TOML, Markdown keep
  their regular modes.

### TypeScript language server

TypeScript 7 (July 2026) is the Go rewrite and no longer ships
`tsserver.js`, so `typescript-language-server` (lsp-mode's `ts-ls`)
can't start with it. The config disables `ts-ls` and uses **tsgo**,
TypeScript 7's native server. On each machine:

1. `M-x package-refresh-contents`, then `M-x package-upgrade RET lsp-mode`
   — lsp-mode needs to be from
   Oct 2026 or later (PR #5113); older versions send `null` fields that
   tsgo rejects ("cannot unmarshal … inlineCompletion").
2. `M-x lsp-install-server RET tsgo` — installs `@typescript/native-preview`
   under `~/.emacs.d/.cache/lsp/`.

Then opening a `.ts`/`.tsx`/`.js`/`.jsx` file starts tsgo: completion
(company), `C-c ,` / `M-?` references, `M-.` definitions, flycheck
diagnostics. If lsp-mode is too old, a `dotfiles` warning says so when it
loads.

tsgo always checks with its own TypeScript 7, never the project's
installed version. A repo still on TypeScript 5 can show errors for
options TS 7 removed, e.g. `TS5102 Option 'baseUrl' has been removed`,
`TS5108 Option 'target=ES5' has been removed`, `moduleResolution: node`,
`esModuleInterop: false`. Those are real TS 7 incompatibilities in the
project's `tsconfig.json`, not an Emacs problem.

### Refactoring the config

`bin/emacs-config-snapshot` prints every binding, variable, dotfiles
function, advice and saved face after loading the config. Diff two runs:

```sh
bin/emacs-config-snapshot > before.txt
# … edit …
LC_ALL=C diff -a before.txt <(bin/emacs-config-snapshot)
```

To test a real startup in batch: `emacs --batch -u $USER --eval '…'`
(`--batch` alone skips early-init and package activation).

## Key reference

### `C-c` personal map

| Key | Command | Key | Command |
|---|---|---|---|
| `C-c a` | magit-status | `C-c b b` | magit-blame |
| `C-c c` | calculator | `C-c d` | duplicate line/region |
| `C-c i`, `C-c '` | iedit-mode (`RET` exits) | `C-c j` | json-pretty-print (region) |
| `C-c n` | flycheck-next-error | `C-c o` | occur |
| `C-c q` / `C-c C-q` | unfill / fill-region | | |
| `C-c r` | revert buffer, no questions | `C-c t` | auto-revert-tail-mode |
| `C-c v` | view-mode | `C-c w` | agent-shell-send-dwim |
| `C-c y` | flyspell buffer | `C-c 0` | insert console log line |
| `C-c h` / `C-c C-h` | highlight phrase / regexp | `C-c k` | Claude Code prefix |
| `C-c ,` | lsp-find-references | `C-c C-p` | compile |
| `C-c -` / `C-c =` | text scale down / up | `C-c TAB` | set tab width (prefix arg) |
| `C-c <up>` / `C-c <down>` | up-list (pushes) / pop back | | |
| `C-c m b` | bind last macro to a key (session; `C-x C-k b`) | `C-c m d` | global-unset-key |

`C-c f` — files and search:

| Key | Command | Key | Command |
|---|---|---|---|
| `C-c f f` | find-file-at-point | `C-c f i` | insert file name |
| `C-c f g` | lgrep | `C-c f a` | ag |
| `C-c f b` | browse-url-at-point | `C-c f d` | find-name-dired |
| `C-c f j` | jump-to-code | `C-c f z f/d/b` | fzf / fzf in dir / fzf buffer |

### Global

| Key | Command | Key | Command |
|---|---|---|---|
| `M-o` | other-window | `M-j` | avy-goto-word-1 (char, then label) |
| `M-=` / `M-+` | expand / contract region | `M-S-<up>/<down>` | move line/region |
| `M-B` | bring-sexp | `M-r` | repeat |
| `M-z` | zap-up-to-char | `M-<delete>` | kill-word |
| `M-'` | search symbol at point | `C-j` | join line backward |
| `C-o` | open line above (`C-u C-o`: 4 lines down, `M-3 C-o`: 3, `M-- C-o`: 1 up) | `C-M-<return>` | unmark-sexp |
| `C-<up>/<down>` | scroll 3 lines | `M-s-<left>/<right>` | previous/next buffer |
| `<f2>` / `M-<f2>` | call macro / on region lines | `<f4>` | end/call macro (Emacs default) |
| `<f9>` / `M-<f9>` | recompile / go to *compilation* | `<f8>` / `S-<f8>` | ECA next attention (/ in project) |
| `C-x n i` | narrow to indirect buffer | `C-x $` | selective display at column |
| `C-h n` | man | `C-z` (GUI) | indent line, next line |
| `M-.` / `M-,` | xref definition / back | `C-c l` (LSP buffers) | LSP prefix |

## For an agent setting up another machine (e.g. the work laptop)

The Nov 2026 modernization (beads epic `dotfiles-117`) changed things that
live outside the repo. After `git pull` on a machine that ran the old
config, do these steps in order, and report what you did. Ask the user
before deleting or installing anything not listed here.

**How to run things.** Use batch Emacs with the real startup:
`emacs --batch -u $USER --eval '…'` (plain `--batch` skips early-init and
package activation). Every command below is written to paste as is: the
Lisp uses `(quote x)` and `(function f)` instead of `'x` / `#'f`, which
would break the shell's single quotes. Ignore "Unable to start the Emacs
server" when an Emacs is already running. If the user's interactive
Emacs is open, ask them to restart it after steps 6–8 (it keeps the old
lsp-mode and grammars loaded).

**Prerequisites**: `ag`, `node`/`npm`, a C compiler (`cc`), network access.

1. **Submodules**: `git submodule update --init`. `git submodule status`
   should list only `lisp/alloy-mode`. Old submodule folders
   (`lisp/tsx-mode`, `lisp/tree-sitter-css-in-js`, `lisp/pycoverage`) may
   be left behind as untracked files: harmless (not on `load-path`); ask
   the user before deleting them.
2. **early-init.el**: if `~/.emacs.d/early-init.el` is missing,
   `ln -s ../dotfiles/emacs/early-init.el ~/.emacs.d/early-init.el`.
   Without it startup shows a `dotfiles` warning and MELPA is missing.
3. **Machine-specific packages**: packages are now declared with
   `use-package`, and `package-selected-packages` is generated from those
   declarations. Before step 5, make sure every package the machine's
   `~/dotfiles/<name>/init.el` uses is declared there with `use-package`
   (`:ensure nil` for local `.el` files). Ask the user if unsure.
4. **Refresh the package archives, then start once** so missing packages
   install (an old archive cache points at versions MELPA has deleted):

   ```sh
   emacs --batch -u $USER --eval '(package-refresh-contents)'
   emacs --batch -u $USER --eval '(princ "ok\n")'
   ```

   The first may print package download errors (startup runs before the
   refresh); what matters is that the second prints `ok` with no
   `Warning`/`Error` lines except the server one.
5. **Remove packages the config no longer declares.** List them:

   ```sh
   emacs --batch -u $USER --eval '(princ (format "%S\n" (mapcar (function package-desc-name) (package--removable-packages))))'
   ```

   Show the list to the user; once they approve, delete them:
   `emacs --batch -u $USER --eval '(package-autoremove t)'`.
6. **lsp-mode must be from Oct 2026 or later.** (The `dotfiles` warning
   only appears when lsp-mode loads, not at startup, so check
   explicitly.)

   ```sh
   emacs --batch -u $USER --eval '(princ (format "%S\n" (package-desc-version (cadr (assq (quote lsp-mode) package-alist)))))'
   ```

   If the first number is below `20261001`:
   `emacs --batch -u $USER --eval '(progn (package-refresh-contents) (package-upgrade (quote lsp-mode)))'`.
7. **TypeScript server (tsgo).** The install runs npm asynchronously, so
   the command waits for it:

   ```sh
   emacs --batch -u $USER --eval '(progn (require (quote lsp-javascript)) (lsp-install-server nil (quote tsgo)) (let ((n 0)) (while (and (< n 300) (seq-some (lambda (p) (and (eq (process-type p) (quote real)) (process-live-p p))) (process-list))) (accept-process-output nil 1) (setq n (1+ n)))) (princ (format "tsgo: %S\n" (ignore-errors (lsp-package-path (quote tsgo))))))'
   ```

   It should print a path to `tsgo`, usually ending in
   `@typescript/native-preview/bin/tsgo`; a `tsgo` already on `PATH`
   (e.g. `/opt/homebrew/bin/tsgo`) is fine too. `nil` means it failed.
   (Give the command a timeout of a few minutes.)
   A stale ts-ls install in `~/.emacs.d/.cache/lsp/npm/`
   (`typescript-language-server`, `typescript`) is unused (ts-ls is
   disabled); ask before deleting it.
8. **Tree-sitter grammars.** The old config installed `typescript` and
   `tsx` from v0.20 pins that don't match Emacs 31's font-lock rules, and
   never installed `dockerfile`. Build these from Emacs's own recipes
   (needs `cc`); `python` is included so step 9 can't stop on an install
   prompt:

   ```sh
   emacs --batch -u $USER --eval '(progn (require (quote typescript-ts-mode)) (require (quote dockerfile-ts-mode)) (dolist (l (quote (typescript tsx dockerfile python))) (treesit-install-language-grammar l)))'
   ```

   Other grammars install on first use, after Emacs asks (interactive
   Emacs only).
9. **Verify.** Save this as `/tmp/verify-emacs.el` and run
   `emacs --batch -u $USER -l /tmp/verify-emacs.el </dev/null`:

   ```elisp
   ;;; -*- lexical-binding: t -*-
   (require 'lsp-mode)           ; before binding its variables in let*
   (let* ((dir (make-temp-file "verify-ts" t))
          (lsp-session-file (expand-file-name "lsp-session" dir))
          (lsp-auto-guess-root t)
          (lsp-enable-suggest-server-download nil) ; no prompt if no server
          (lsp-restart 'ignore))        ; no prompt if the server crashes
     (with-temp-file (expand-file-name "package.json" dir) (insert "{}"))
     (with-temp-file (expand-file-name "a.ts" dir) (insert "const x: number = 1;\n"))
     (with-current-buffer (find-file-noselect (expand-file-name "a.ts" dir))
       (lsp)
       (let ((n 0))
         (while (and (< n 60) lsp--buffer-workspaces
                     (not (and lsp--buffer-workspaces
                               (eq (lsp--workspace-status (car lsp--buffer-workspaces))
                                   'initialized))))
           (accept-process-output nil 0.5) (setq n (1+ n))))
       (let ((ws (car lsp--buffer-workspaces)))
         (princ (format "ts: mode=%S server=%S status=%S company=%S\n" major-mode
                        (and ws (lsp--client-server-id (lsp--workspace-client ws)))
                        (and ws (lsp--workspace-status ws))
                        (bound-and-true-p company-mode))))
       (font-lock-ensure)
       (ignore-errors (lsp-workspace-shutdown (car lsp--buffer-workspaces))))
     (delete-directory dir t))
   (with-temp-buffer
     (setq buffer-file-name "/tmp/verify.py") (set-auto-mode)
     (princ (format "py: mode=%S flycheck=%S\n" major-mode (bound-and-true-p flycheck-mode)))
     (setq buffer-file-name nil))
   (with-temp-buffer
     (setq buffer-file-name "/tmp/Dockerfile") (set-auto-mode)
     (princ (format "docker: mode=%S\n" major-mode))
     (setq buffer-file-name nil))
   (princ (format "font-lock mismatch warning: %S\n"
                  (and (get-buffer "*Warnings*")
                       (with-current-buffer "*Warnings*"
                         (string-match-p "treesit-font-lock-rules-mismatch" (buffer-string)))
                       t)))
   ```

   Expected: `ts: mode=typescript-ts-mode server=tsgo status=initialized company=t`,
   `py: mode=python-ts-mode flycheck=t`, `docker: mode=dockerfile-ts-mode`,
   `font-lock mismatch warning: nil`. If something's off, report it:
   - `server=nil`: tsgo isn't installed (redo step 7), or it crashed on
     start (a "tsgo has exited (exited abnormally …)" line appears above;
     check that the binary runs: `<path from step 7> --version`).
   - `server=tsgo status=starting`: tsgo rejected the connection, usually
     lsp-mode too old (redo step 6).
   - another server, e.g. `graphql-lsp` or `eslint`: those are add-on
     clients that only show up when tsgo isn't running, so this also
     means tsgo is missing (redo step 7).

Nothing here needs committing: these are machine-local steps.
