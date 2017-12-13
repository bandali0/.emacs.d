# Bash-like navigation
#export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export WORDCHARS='*?-[]~=&;!#$%^(){}<>'

fpath+=~/.zfunc

disable -r time       # disable shell reserved word
alias time='time -p'  # -p for POSIX output

# rehash if last command was pacaur or pacman
# (so that zsh picks up changes in $PATH immediately)
TRAPUSR1() { rehash}; precmd() { [[ $history[$[ HISTCMD -1 ]] == *(pacaur|pacman)* ]] && killall -USR1 zsh }



#
# User configuration sourced by interactive shells
#

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi


ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
#ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold' 

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-autopair/autopair.zsh

# aliases
alias mpv="mpv --ytdl-format mp4"
alias mv="mv -iv"
alias cp="cp -iv"
alias scl=systemctl
alias jcl=journalctl
alias m="mbsync -Va; getmail; notmuch new"
alias best="youtube-dl -f best"
