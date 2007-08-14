# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything:
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Prompt
PS1="\w\$ "
