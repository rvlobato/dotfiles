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
HISTDIR="$HOME/Desktop/.bash_history"
HISTHOST="${HOSTNAME%%.*}"
HISTFILE="$HISTDIR/$HISTHOST.history"
HISTSIZE=5000000
HISTFILESIZE=5000000
HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='history:hibernate:exit:rm*:cd*:more*:ls'
shopt -s histappend cmdhist

[[ -d $HISTDIR ]] || mkdir -p -- "$HISTDIR"
[[ -e $HISTFILE ]] || : > "$HISTFILE"

__hist_sig() {
    stat -c '%n %s %Y' -- "$HISTDIR"/*.history 2>/dev/null
}

__hist_reload() {
    local merged="${XDG_RUNTIME_DIR:-/tmp}/bash-history-$$"
    local -a files=()
    local f
    for f in "$HISTDIR"/*.history; do
        [[ -f $f && $f != *sync-conflict* && $f != "$HISTFILE" ]] && files+=("$f")
    done
    files+=("$HISTFILE")
    cat -- "${files[@]}" | tac | awk 'NF && !seen[$0]++' | tac > "$merged"
    history -c
    history -r "$merged"
    rm -f -- "$merged"
}

__hist_sync() {
    local sig
    sig=$(__hist_sig)
    history -a
    [[ $sig == "$__HIST_SIG" ]] || __hist_reload
    __HIST_SIG=$(__hist_sig)
}

hist-compact() {
    local tmp="${XDG_RUNTIME_DIR:-/tmp}/bash-history-compact-$$"
    history -a
    tac -- "$HISTFILE" | awk 'NF && !seen[$0]++' | tac > "$tmp" && cat -- "$tmp" > "$HISTFILE"
    rm -f -- "$tmp"
    __HIST_SIG=
}

[[ $PROMPT_COMMAND == *__hist_sync* ]] || PROMPT_COMMAND="__hist_sync${PROMPT_COMMAND:+; $PROMPT_COMMAND}"

#autocd
shopt -s autocd

#------------------------------
# Alias
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
PS1='\[\e[0;34m\][\[\e[01;33m\]\u\[\e[0;31m\]@\[\e[01;33m\]\h\[\e[01;35m\] \W\[\e[0;36m\]]\[\e[m\] \$ '

toggle-theme() {
    local pointer="$HOME/.config/alacritty/active_theme.toml"
    local theme_dir="$HOME/.config/alacritty/themes/themes"

    local light="$theme_dir/modus_operandi.toml"
    local dark="$theme_dir/modus_vivendi.toml"

    local current=$(gsettings get org.gnome.desktop.interface color-scheme)

    if [[ "$current" == *'prefer-dark'* ]]; then
        # Switch to Light Theme
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
        gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
        ln -sf ~/.config/fuzzel/themes/light.ini ~/.config/fuzzel/fuzzel.ini
        ln -sf "$light" "$pointer"
    else
        # Switch to Dark Theme
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
        gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
        ln -sf ~/.config/fuzzel/themes/dark.ini ~/.config/fuzzel/fuzzel.ini
        ln -sf "$dark" "$pointer"
    fi
}
