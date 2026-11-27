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
| `C-c l` `font-lock-fontify-buffer` | `font-lock-update` (same idea). In LSP buffers `C-c l` is the LSP prefix |
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
| `C-c c` | Claude Code prefix | `C-c d` | duplicate line/region |
| `C-c i`, `C-c '` | iedit-mode (`RET` exits) | `C-c j` | json-pretty-print (region) |
| `C-c n` | flycheck-next-error | `C-c o` | occur |
| `C-c q` / `C-c C-q` | unfill / fill-region | | |
| `C-c r` | revert buffer, no questions | `C-c t` | auto-revert-tail-mode |
| `C-c v` | view-mode | `C-c w` | agent-shell-send-dwim |
| `C-c y` | flyspell buffer | `C-c 0` | insert console log line |
| `C-c h` / `C-c C-h` | highlight phrase / regexp | | (`calculator` on `C-c c` is shadowed by Claude Code) |
| `C-c ,` | lsp-find-references | `C-c C-p` | compile |
| `C-c -` / `C-c =` | text scale down / up | `C-c <tab>` | set tab width (prefix arg; GUI only) |
| `C-c <up>` / `C-c <down>` | up-list (pushes) / pop back | `C-c l` | font-lock-update |
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
