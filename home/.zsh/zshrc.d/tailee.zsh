# ls.
alias ls='ls -FG --color'

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

function y() {
  cd ~/misc/tmp-music
  youtube-dl -f bestaudio $1
  for f in *.m4a; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 "${f%.m4a}.mp3"; rm "$f"; done
}

case $- in *i*)
      if [ -z "$TMUX" ]; then exec tmux -2; fi;;
esac

export LS_COLORS=$LS_COLORS:"di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:"

alias emacs='emacs -nw'
