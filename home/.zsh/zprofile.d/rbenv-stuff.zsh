# use homebrew dirs for rbenv instead of ~/.rbenv
export RBENV_ROOT=/usr/local/var/rbenv

# enable shims and auto completions
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi