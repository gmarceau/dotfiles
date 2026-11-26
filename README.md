# dotfiles
Assorted Linux Configuration Files

## Emacs setup

The config lives in `emacs/`. Link it from the home directory:

```sh
ln -s dotfiles/.emacs ~/.emacs                                  # .emacs -> emacs/core.el
ln -s ../dotfiles/emacs/early-init.el ~/.emacs.d/early-init.el
```

Packages are declared with `use-package` and install themselves on the
first start (network needed). Machine-specific config goes in
`~/dotfiles/<name>/init.el`, which `core.el` loads last.
