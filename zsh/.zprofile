#PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
PATH=$HOME/.gem/ruby/2.4.0/bin:$PATH
export PATH=$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share
export MAILDIR="$HOME/mail"
export CVS_RSH=ssh
export MATHMODELS=$HOME/src/eiffel/mathmodels
export RUST_SRC_PATH=~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

# Eiffel2Java stuff
export JAVA_HOME=/usr/lib/jvm/default
export CPATH=$CPATH:"$JAVA_HOME/include:$JAVA_HOME/include/linux"
export LIBRARY_PATH=$LIBRARY_PATH:"$JAVA_HOME/jre/lib/amd64/server"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$JAVA_HOME/jre/lib/amd64/server"

export PATH="$HOME/usr/build/pvs:$PATH"
export SBCLISP_HOME=/usr/share/sbcl-source
export PVS_LIBRARY_PATH="$HOME/usr/build/pvs/nasalib"


 #if [ -e /home/amin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/amin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
