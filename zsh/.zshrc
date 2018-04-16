# Bash-like navigation
#export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
#export WORDCHARS='*?-[]~=&;!#$%^(){}<>'
export WORDCHARS='*?[]~=&;!#$%^(){}<>'
#ZLE_SPACE_SUFFIX_CHARS=$'|&'

#disable -r time       # disable shell reserved word
#alias time='time -p'  # -p for POSIX output

# rehash if last command was pacaur or pacman
# (so that zsh picks up changes in $PATH immediately)
TRAPUSR1() { rehash}; precmd() { [[ $history[$[ HISTCMD -1 ]] == *(pacaur|pacman)* ]] && killall -USR1 zsh }

#
# User configuration sourced by interactive shells
#

# Change default zim location
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Start zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
#ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold' 

setopt globdots

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-autopair/autopair.zsh


### fzf ###

source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

# Accept history selection instead of putting it on
# the command line
fzf-history-widget-accept() {
  fzf-history-widget
  zle accept-line
}
#zle     -N   fzf-history-widget-accept
#bindkey '^R' fzf-history-widget-accept

# alt+c preview
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

### fzf ###


# aliases
alias mpv="mpv --ytdl-format mp4"
alias mv="mv -iv"
alias cp="cp -iv"
alias scl=systemctl
alias jcl=journalctl
alias m="mbsync -Va; getmail; notmuch new"
alias best="youtube-dl -f best"
alias ace="mosh amin@ace.aminb.org"
alias nix="ssh amin@aminb.org"

aur() {
   cd ~/usr/build
   git clone https://aur.archlinux.org/${1}.git
   cd ${1}
}

# i-beam cursor
echo -e "\033[5 q"
#echo -e "\033[6 q"
