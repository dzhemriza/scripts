#!/bin/bash
# Name: bashrc.sh
# Description: Script initializing the bash environment
#
# Usage:
# Put the line below ~/.bashrc file
# . ~/scripts/bashrc.sh

# Alias declarations
alias ll="ls -lah"
alias l="ls -lah"
alias grep="grep --color=auto"
alias vim="vim -X"

EMACS=$( dirname "${BASH_SOURCE[0]}" )/emacs/emacs

alias e="$EMACS -nw"
alias ew="$EMACS &"

# Java stuff

# Make sure you have the following symlinks
JAVA_HOME=~/apps/default-jdk
#M2_HOME=~/apps/maven

export JAVA_HOME
#export M2_HOME

export PATH=$JAVA_HOME/bin:$PATH
#export PATH=$M2_HOME/bin:$PATH
