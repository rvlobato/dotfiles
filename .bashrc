# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.
# Enable checkwinsize so that bash will check the terminal size when
shopt -s checkwinsize

# Disable completion when the input buffer is empty.  i.e. Hitting tab
# and waiting a long time for bash to expand all of $PATH.
shopt -s no_empty_cmd_completion

# History
HISTSIZE=5000000
SAVEHIST=5000000

# Disable duplicate command from history
HISTCONTROL=ignoreboth:erasedups

# remove commands from history
HISTIGNORE='history:hibernate:exit:rm*:cd*:more*:ls'

# After each command, append to the history file and reread it
PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

#autocd
shopt -s autocd

for sh in /etc/bash/bashrc.d/* ; do
	[[ -r ${sh} ]] && source "${sh}"
done

#------------------------------
# Alias
alias pkglist-dif='sdiff -s <(pacman -Qqe) $HOME/.config/pkglist.txt'
[ -f "$HOME/.aliases" ] && source "$HOME/.aliases"

# Dir colors
# LS_COLORS="$(vivid generate solarized-dark)"
# export LS_COLORS

# PATH
PATH=$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/python/bin

#------------------------------
# Functions
#------------------------------

#mkdir cd
mkcd ()
{
  mkdir -p -- "$1" && cd -P -- "$1"
}

#-------------
#Powerline
#powerline-daemon -q
#POWERLIN1E_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/share/powerline/bindings/bash/powerline.sh


#----------------------------------------
## fzf
eval "$(fzf --bash)"

# Use ~~ as the trigger sequence instead of the default **
export FZF_COMPLETION_TRIGGER='~~'

# Options to fzf command
export FZF_COMPLETION_OPTS='--border --info=inline'

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

#------------------------------
# Prompt
#------------------------------
PS1='\[\e[0;34m\][\[\e[01;33m\]\u\[\e[0;31m\]@\[\e[01;33m\]\h\[\e[01;35m\] \W\[\e[0;36m\]]\[\e[m\]\$ '

# Added by Pear Runtime, configures system with Pear CLI
export PATH="/home/ronaldo/.config/pear/bin":$PATH
