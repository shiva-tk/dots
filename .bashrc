#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export VISUAL=nvim
export EDITOR="$VISUAL"

alias ls='ls --color=auto'
PS1="[\w]\n\\u@\h -> "

neofetch
