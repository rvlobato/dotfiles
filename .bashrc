#Command not found
#source /usr/share/doc/pkgfile/command-not-found.bash

# If not running, don't do anything
if [[ $- != *i* ]] ; then
	 	return
fi

# Enable checkwinsize so that bash will check the terminal size when
shopt -s checkwinsize

# Disable completion when the input buffer is empty.  i.e. Hitting tab
# and waiting a long time for bash to expand all of $PATH.
shopt -s no_empty_cmd_completion

# History
HISTSIZE=5000000
SAVEHIST=5000000

##Disable duplicate command
export HISTCONTROL=ignoredups

## Enable history appending instead of overwriting when exiting.  #139609
shopt -s histappend

#autocd
shopt -s autocd

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.
use_color=false
if type -P dircolors >/dev/null ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	LS_COLORS=
	if [[ -f ~/.dir_colors ]] ; then
		# If you have a custom file, chances are high that it's not the default.
		used_default_dircolors="no"
		eval "$(dircolors -b ~/.dir_colors)"
	elif [[ -f /etc/DIR_COLORS ]] ; then
		# People might have customized the system database.
		used_default_dircolors="maybe"
		eval "$(dircolors -b /etc/DIR_COLORS)"
	else
		used_default_dircolors="yes"
		eval "$(dircolors -b)"
	fi
	if [[ -n ${LS_COLORS:+set} ]] ; then
		use_color=true
	fi
	unset used_default_dircolors
else
	# Some systems (e.g. BSD & embedded) don't typically come with
	# dircolors so we need to hardcode some terminals in here.
	case ${TERM} in
	[aEkx]term*|rxvt*|gnome*|konsole*|screen|cons25|*color) use_color=true;;
	esac
fi

if ${use_color} ; then
	if [[ ${EUID} == 0 ]] ; then
	    PS1+='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
	else
	    PS1="\[$(tput setaf 2)\][\[$(tput setaf 3)\]\u\[$(tput setaf 1)\]@\[$(tput setaf 3)\]\h \[$(tput setaf 6)\]\W\[$(tput setaf 2)\]]\[$(tput setaf 4)\]\\$ \[$(tput sgr0)\]"
	    	fi

	else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1+='\u@\h \W \$ '
	else
		PS1+='\u@\h \w \$ '
	fi
fi

for sh in /etc/bash/bashrc.d/* ; do
	[[ -r ${sh} ]] && source "${sh}"
done

#------------------------------
# Alias stuff
#------------------------------
	alias ls='ls --color=auto'
	alias ll='ls --color -lh'
	alias grep='grep --colour=always'
	alias egrep='egrep --colour=auto'
	alias fgrep='fgrep --colour=auto'
	alias emacs='emacs -nw'
	alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

#Environment variables
#---------------------------------
	export BROWSER=firefox
	export DE=gnome	
	export EDITOR=vim
	export XDG_CONFIG_HOME=$HOME/.config
	export XDG_DATA_HOME=$HOME/.local/share
	export GCC_COLORS=1
	export HOME_LORENE=$HOME/gCloudDrive/research/codes/numerical_relativity/Lorene
	export PLUTO_DIR=$HOME/gCloudDrive/research/codes/numerical_relativity/pluto/pluto-4.2/PLUTO
	export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab
	export R_LIBS=$HOME/Documents/R/library
	export PATH=$PATH:$HOME/.local/bin

#-----------------
#More alis
## pacman
  alias pacup='sudo pacman -Syu '
  alias pacs='pacman -Ss'
  alias pacr='sudo pacman -Rnsc'
  
## git
  alias gam='git commit --amend '
  alias gcm='git checkout master'
  alias gfu='git fetch upstream'
  alias grm='git rebase -i master '
  alias gm='git merge '


# moving in dirs
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."

# Dir colors
eval $(dircolors -b $HOME/.dircolors)

#------------------------------
# Functions
#------------------------------

#tilix
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi

#fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash


##pip user
function pip() {
      if [[ "$1" == "install" ]]; then
          shift 1
          command pip install --user "$@"
      else
          command pip "$@"
      fi
}
