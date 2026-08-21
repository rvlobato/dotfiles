export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export XDG_DATA_DIRS="$XDG_DATA_HOME:$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share"

export BROWSER=epiphany
export EDITOR=emacs
export VISUAL=emacs
export QT_QPA_PLATFORM=wayland
export ANKI_WAYLAND=1
export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab

export PATH="${PATH}:$HOME/.local/bin:$HOME/.cargo/bin"

export HOME_LORENE="$HOME/Desktop/resrch/codes/numerical_relativity/Lorene"
export PLUTO_DIR="$HOME/Desktop/resrch/codes/numerical_relativity/pluto/pluto-4.3/PLUTO"
export MESA_DIR="$HOME/Desktop/resrch/codes/mesa/mesa"

export MESON_BUILDDIR="$HOME/.local"
export AERIE_INSTALLATION_DIR="$HOME/Projects/SWGO/aerie"
export SPACK_PYTHON=/usr/bin/python3.12

export JULIA_NUM_THREADS=$(lscpu -b -p=Core,Socket | grep -v '^#' | sort -u | wc -l)

[[ -f ~/.bashrc ]] && . ~/.bashrc
