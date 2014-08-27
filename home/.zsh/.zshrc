# `.zshrc' is sourced in interactive shells. It should contain commands to set
# up aliases, functions, options, key bindings, etc.

# Source Prezto defaults.
source ${ZDOTDIR:-$HOME}/.zprezto/runcoms/zshrc

# Source `zshrc.d/*.zsh` scripts.
for filename in ${ZDOTDIR:-$HOME}/zshrc.d/*.zsh(N); do
	source $filename
done
