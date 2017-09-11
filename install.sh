#!/bin/bash

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

if [ -e $HOME/.tmux.conf ]; then
    echo "...backing up .tmux.conf file to .tmux.conf.BACKUP"
    mv $HOME/.tmux.conf $HOME/.tmux.conf.BACKUP
fi

echo "...linking .tmux.conf"
ln -s $DIR/tmux/tmux.conf $HOME/.tmux.conf
