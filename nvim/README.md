
## Installation

I lives in `~/.config/nvim`

NeoVim configuration, use vim-plug to manage plugins.

```
# install nvim nightly. appimage is the fastest way.
# curl -o /usr/local/bin/nvim -LO https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage && chmod +x /usr/local/bin/nvim
```

```
# clone this repo
mkdir -p ~/.config && cd ~/.config && git clone --depth 1 https://github.com/sekirocc/nvim.git

# install vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


# install python2 & python3 support
python3 -m pip install --user --upgrade pynvim
python2 -m pip install --user --upgrade pynvim

# start nvim and install plugins
nvim +PlugInstall +qall
```


```
# Install tools(fd, ag/rg, fzf)
#
# some advice:
snap install ripgrep --classic
brew install fd

#
# install nerd font
brew tap homebrew/cask-fonts
brew install font-dejavu-sans-mono-nerd-font

```

## local override

put customization to ~/.vimrc_local.lua

```
cat ~/.vimrc_local.lua

vim.opt.clipboard:prepend("unnamed,unnamedplus")
```



# plugins will be installed in ~/.nvim

## Golang

install vim-go binaries

```
export GOPROXY=https://goproxy.io
nvim +GoInstallBinaries
```

## For cpp
```
install clangd
```


## For cscope

first install cscope & ctags
```
# 1. pacman
pacman -Sy cscope ctags
#
#
# snap install universal-ctags --classic
# brew install --HEAD universal-ctags/universal-ctags/universal-ctags
# brew link --overwrite universal-ctags
# brew install cscope
```

use this script to generate cscope files

```bash
#!/bin/sh
find . -path "./.ccls-cache" -prune -o -path "./.git" -prune -o -path "./build" -prune -o -path "./cmake-build-debug" -prune \
    -o -name "*.h" -o -name "*.hh" -o -name "*.hpp" \
    -o -name "*.c" -o -name "*.cc" -o -name "*.cpp"  | \
    egrep -v '.ccls-cache|.git|build|cmake-build-debug' | sort > .cscope.files
cscope -bkq -i .cscope.files -f .cscope.out
ctags --c++-kinds=+p --fields=+iaS --exclude=".ccls-cache/*" --exclude="build" --exclude="cmake-build-debug" -R -f .tags
```

it will generate the following files:

```
.cscope.files
.cscope.out
.cscope.out.in
.cscope.out.po
.tags
```
you may need to ignore them in .gitignore

in our vimrc, use `<leader>s s` to jump to symbol

Happy hacking ~

