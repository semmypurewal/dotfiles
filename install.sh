#!/bin/bash

SELF="${BASH_SOURCE[0]}"
SELF_DIR="$(cd "$(dirname $SELF)" ; pwd -P )"

function backup () {
    echo "backing up $1 to $1.BACKUP"
    mv "$1" "$1.BACKUP"
}

function backup-if-exists () {
    if [ -e "$1" ]; then
        check-for-backup $1
        backup "$1"
    fi
}

function check-for-backup () {
    if [ -e "$1.BACKUP" ]; then
        echo "Oops! found $1.BACKUP. You probably don't want me to delete this."
        echo "Move $1.BACKUP so I can safely write a new one."
        exit 1
    fi
}

function soft-link () {
    echo "linking $1 to $2"
    ln -s "$1" "$2"
}

backup-if-exists $HOME/.emacs
backup-if-exists $HOME/.emacs.d/elpa
backup-if-exists $HOME/.tmux.conf

if [ ! -e $HOME/.emacs.d ]; then
    echo "...creating emacs.d"
    mkdir $HOME/.emacs.d
fi

soft-link $SELF_DIR/emacs/emacs $HOME/.emacs
soft-link $SELF_DIR/emacs/elpa $HOME/.emacs.d/elpa
soft-link $SELF_DIR/tmux/tmux.conf $HOME/.tmux.conf
