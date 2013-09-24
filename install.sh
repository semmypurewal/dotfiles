#!/bin/bash

# following line borrwed from
# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
DIR="$( cd "$( dirname "$0" )" && pwd )"

echo "running install script from $DIR"
if [ -e $HOME/.emacs ]; then
    echo "...backing up .emacs file to .emacs.BACKUP"
    mv $HOME/.emacs $HOME/.emacs.BACKUP
fi

if [ -e $HOME/.emacs.d/lib ]; then
    echo "...backing up .emacs.d/lib directory to .emacs.d/lib.BACKUP"
    mv $HOME/.emacs.d/lib $HOME/.emacs.d/lib.BACKUP
fi

echo "...linking .emacs"
ln -s $DIR/emacs/emacs $HOME/.emacs

if [ ! -e $HOME/.emacs.d ]; then
    echo "...creating emacs.d"
    mkdir $HOME/.emacs.d
fi

echo "../linking .emacs.d/lib"
ln -s $DIR/emacs/lib $HOME/.emacs.d/lib
