#! /bin/bash

# clone my prelude fork
export PRELUDE_URL="https://github.com/aminb/prelude.git" && \
    curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
cd $HOME/.emacs.d
git remote set-url origin git@github.com:aminb/prelude.git
