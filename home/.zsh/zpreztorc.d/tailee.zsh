#
# General
#

# Set the Prezto modules to load (browse modules).
# The order matters.
# `environment` must be first.
# `completion` must be after `utility`.
# `fasd` must be after `completion`.
# `syntax-highlighting` must be second last, before `prompt` and `history-substring-search`.
# `history-substring-search` must be after `syntax-highlighting`.
zstyle ':prezto:load' pmodule \
	'environment' \
	'archive' \
	'git' \
	'homebrew' \
	'osx' \
	'python' \
	'rsync' \
	'terminal' \
	'editor' \
	'history' \
	'directory' \
	'spectrum' \
	'utility' \
	'completion' \
	'fasd' \
	'syntax-highlighting' \
	'history-substring-search' \
	'prompt'

# Auto convert .... to ../..
zstyle ':prezto:module:editor' dot-expansion 'yes'

#
# Syntax Highlighting
#

# Set syntax highlighters.
# By default, only the main highlighter is enabled.
zstyle ':prezto:module:syntax-highlighting' highlighters \
	'main' \
	'brackets' \
	'pattern' \
	'cursor' \
	'root'

#
# Terminal
#

# Auto set the tab and window titles.
zstyle ':prezto:module:terminal' auto-title 'yes'
