# `.zlogout' is sourced when login shells exit.

# Source Prezto defaults.
source ${ZDOTDIR:-$HOME}/.zprezto/runcoms/zlogout

# Source `zlogout.d/*.zsh` scripts.
for filename in ${ZDOTDIR:-$HOME}/zlogout.d/*.zsh(N); do
	source $filename
done
