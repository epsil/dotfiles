Various configuration files.

Managed using [rcm](http://thoughtbot.github.io/rcm/rcm.7.html).
Update symlinks with `rcup -v` (dry run with `lsrc`).

---

Or execute the following in the `~/` directory, with `dotfiles` in `~/dotfiles`:

```
ln -b -s dotfiles/bash_aliases ~/.bash_aliases
ln -b -s dotfiles/bash_env ~/.bash_env
ln -b -s dotfiles/bashrc ~/.bashrc
ln -b -s dotfiles/rcrc ~/.rcrc
ln -b -s dotfiles/vimrc ~/.vimrc
ln -b -s dotfiles/zshrc ~/.zshrc
```

Git:

```
cp dotfiles/gitconfig ~/.gitconfig
```

Emacs:

```
ln -b -s dotfiles/emacs ~/.emacs
ln -b -s dotfiles/emacs.d ~/.emacs.d
```

Or if you use Spacemacs:

```
ln -b -s dotfiles/spacemacs ~/.spacemacs
```
