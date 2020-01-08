pacman -Qqe > pkglist.txt && dotfiles commit -am "pkglist update" && dotfiles push github master && dotfiles push gitlab master
