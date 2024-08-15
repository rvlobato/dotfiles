plugins=(… zsh-completions)
autoload -U compinit && compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots)
autoload -U colors && colors

#Sources:
##Aliases
[ -f $HOME/.aliases ] && source $HOME/.aliases

#Plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

#FZF
source /usr/share/fzf/key-bindings.zsh 2>/dev/null
source /usr/share/fzf/completion.zsh 2>/dev/null

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
LS_COLORS="$(vivid generate molokai)"
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

# auto cd
setopt autocd

hist_dedup() {
  sort ~/.histfile > ~/.histfile.old
  uniq ~/.histfile.old > ~/.histfile
}

#------------------------------
# Powerline
#powerline-daemon -q
#. /usr/share/powerline/bindings/zsh/powerline.zsh

#------------------------------
# Prompt
#------------------------------
PS1="%B%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[red]%}@%{$fg[yellow]%}%M %{$fg[magenta]%}%1~%{$fg[blue]%}]%{$reset_color%}$%b "

# Added by Pear Runtime, configures system with Pear CLI
export PATH="/home/ronaldo/.config/pear/bin":$PATH
