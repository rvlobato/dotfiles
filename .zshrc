plugins=(… zsh-completions)
autoload -U compinit && compinit

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#---------------------------------------
#fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

# Use ~~ as the trigger sequence instead of the default **
export FZF_COMPLETION_TRIGGER='~~'

# Options to fzf command
export FZF_COMPLETION_OPTS='+c -x'

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
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=500000
SAVEHIST=500000

#-----------------------------
# Dircolors
#-----------------------------
LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

#------------------------------
# History
#------------------------------
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

#------------------------------
# Alias stuff
#------------------------------
alias ls="ls --color -F"
alias ll="ls --color -lh"
alias grep="grep --color=always"
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'	
alias emacs='emacs -nw'
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

export BROWSER=firefox
export DE=gnome
export EDITOR=vim
export HOME_LORENE=$HOME/gCloudDrive/research/codes/numerical_relativity/Lorene
export PLUTO_DIR=$HOME/gCloudDrive/research/codes/numerical_relativity/pluto/pluto-4.2/PLUTO
export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab
export R_LIBS=$HOME/Documents/R/library
export PATH=$PATH:$HOME/.local/bin

# pacman
alias pacup='sudo pacman -Syu '
alias pacs='pacman -Ss'
alias pacr='sudo pacman -Rnsc'

# git
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

hist_dedup() {
  sort ~/.histfile > ~/.histfile.old
  uniq ~/.histfile.old > ~/.histfile
}

#------------------------------
# Prompt
#------------------------------
setprompt () {
        # load some modules
        autoload -U zsh/terminfo # Used in the colour alias below
        # Use colorized output, necessary for prompts and completions.
        autoload -U colors && colors
        setopt prompt_subst

        # make some aliases for the colours: (coud use normal escap.seq's too)
        for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
                eval PR_$color='%{$fg[${(L)color}]%}'
        done
        PR_NO_COLOR="%{$terminfo[sgr0]%}"

        # Check the UID
        if [[ $UID -ge 1000 ]]; then # normal user
                eval PR_USER='${PR_GREEN}%n${PR_NO_COLOR}'
                eval PR_USER_OP='${PR_GREEN}%#${PR_NO_COLOR}'
        elif [[ $UID -eq 0 ]]; then # root
                eval PR_USER='${PR_RED}%n${PR_NO_COLOR}'
                eval PR_USER_OP='${PR_RED}%#${PR_NO_COLOR}'
        fi      

        # Check if we are on SSH or not  --{FIXME}--  always goes to |no SSH|
        if [[ -z "$SSH_CLIENT"  ||  -z "$SSH2_CLIENT" ]]; then 
                eval PR_HOST='${PR_GREEN}%M${PR_NO_COLOR}' # no SSH
        else 
                eval PR_HOST='${PR_YELLOW}%M${PR_NO_COLOR}' #SSH
        fi
        # set the prompt
        PS1=$'${PR_CYAN}[${PR_USER}${PR_CYAN}@${PR_HOST}${PR_CYAN}][${PR_BLUE}%~${PR_CYAN}]${PR_USER_OP} '
        #PS2=$'%_>'
}
setprompt

#------------------------------
# Functions
#------------------------------

#pip user
function pip() {
        if [[ "$1" == "install" ]]; then
            shift 1
            command pip install --user "$@"
        else
            command pip "$@"
        fi
}
      
