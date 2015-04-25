#! /bin/zsh

# clone my prezto fork
git clone --recursive https://github.com/aminb/prezto.git $HOME/.zprezto
cd $HOME/.zprezto
git remote set-url origin git@github.com:aminb/prezto.git

# setup the symlinks
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
