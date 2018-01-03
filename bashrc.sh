#!/bin/bash
# Name: bashrc.sh
# Description: Script initializing the bash environment
#
# Usage:
# Put the line below ~/.bashrc file
# . ~/scripts/bashrc.sh

# Alias declarations
alias ll="ls -la"
alias l="ls -la"
alias grep="grep --color=auto"
alias vim="vim -X"

EMACS=$( dirname "${BASH_SOURCE[0]}" )/emacs/emacs

alias e="$EMACS -nw"
alias ew="$EMACS &"
