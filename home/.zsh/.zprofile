# `.zprofile' is similar to `.zlogin', except that it is sourced before
# `.zshrc'. `.zprofile' is meant as an alternative to `.zlogin' for ksh fans;
# the two are not intended to be used together, although this could certainly
# be done if desired.

# Source Prezto defaults.
source ${ZDOTDIR:-$HOME}/.zprezto/runcoms/zprofile

# Source `zprofile.d/*.zsh` scripts.
for filename in ${ZDOTDIR:-$HOME}/zprofile.d/*.zsh(N); do
	source $filename
done
