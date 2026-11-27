# dotfiles
Assorted Linux Configuration Files

## Emacs setup

The config lives in `emacs/` (details in `emacs/README.md`). After cloning,
fetch the submodules (`git submodule update --init`), then link it from the
home directory:

```sh
ln -s dotfiles/.emacs ~/.emacs                                  # .emacs -> emacs/core.el
ln -s ../dotfiles/emacs/early-init.el ~/.emacs.d/early-init.el
```

Packages are declared with `use-package` and install themselves on the
first start (network needed). Machine-specific config goes in
`~/dotfiles/<name>/init.el`, which `core.el` loads last.

### Work laptop: `compile-command` moved out of `custom.el` (Nov 2026)

`custom.el` used to set `compile-command` to `"tg plan"` for every
machine. It's gone from the shared config, so on the work laptop add
this line to `~/dotfiles/<name>/init.el`:

```elisp
(setq-default compile-command "tg plan")
```

The other settings that were in `custom.el` now live in the topic files
(`core.el`, `editing.el`, `dev-tools.el`, …); `custom.el` only holds what
Customize writes (theme, font, safe local variables).
