#! /bin/zsh

# clone my prezto fork
git clone --recursive https://git.aminb.org/prezto $HOME/.zprezto

# setup the symlinks
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
