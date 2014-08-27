# ls.
alias ls='ls -FG'

# ps.
alias ps='ps -o pid,rss,pcpu,command'

# SourceTree.
alias st='/Applications/SourceTree.app/Contents/Resources/stree'

# Supervisor.
# alias sup='supervisord -c $HOME/etc/supervisor/supervisord.conf -d $HOME/etc/supervisor'
# alias supctl='supervisorctl -c $HOME/etc/supervisor/supervisord.conf '

# Sublime Text.
# alias sub='"/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"'

# Fix crash on `workon` tab completion. Caused by Prezto using
# `virtualenvwrapper_lazy.sh`.
if (( $+commands[virtualenvwrapper.sh] )); then
  source "$commands[virtualenvwrapper.sh]"
fi

# fasd stuff
eval "$(fasd --init auto)"
alias v='f -e vim'

# take
function take() {
  mkdir -p $1
  cd $1
}

