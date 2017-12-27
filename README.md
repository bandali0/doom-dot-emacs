# dot-emacs

My `init.el` and custom private modules for [doom-emacs][doom-emacs].

## Install

First, clone [doom-emacs][doom-emacs]; then clone this repo and set up the
symlinks:

``` sh
git clone https://github.com/hlissner/doom-emacs.git ~/.emacs.d
git clone https://git.aminb.org/amin/dot-emacs.git ~/usr/dot-emacs
cd ~/.emacs.d
git checkout develop  # this repo is meant to be used with Doom's develop branch
ln -s ~/usr/dot-emacs/init.el .
ln -s ~/usr/dot-emacs/modules/private/* .
make install
make compile
```

If you don't want to use a certain module, feel free to omit it when symlinking
the rest, and also remove its name from the bottom of `init.el`.


[doom-emacs]: https://github.com/hlissner/doom-emacs

