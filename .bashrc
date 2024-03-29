#Command not found
#source /usr/share/doc/pkgfile/command-not-found.bash

# If not running interactively, do not do anything
[[ $- != *i* ]] && return

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
##fzf
source /usr/share/fzf/key-bindings.bash 2>/dev/null
source /usr/share/fzf/completion.bash 2>/dev/null

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
