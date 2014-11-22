# Base16 Shell
#BASE16_SCHEME="thewildhunt"
#BASE16_SHELL="$HOME/.theme/base16-$BASE16_SCHEME.dark.sh"
#[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

PATH="$HOME/.local/bin:$PATH"
#PATH="`ruby -e 'print Gem.user_dir'`/bin:$PATH"
PATH="/opt/android-sdk/platform-tools:$PATH"
#source ~/.profile

# take
function take() {
 mkdir -p $1
 cd $1
}

function y() {
 cd ~/music
 youtube-dl --extract-audio --audio-format mp3 --audio-quality 0 $1
}

#eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh) ; export GPG_AGENT_INFO SSH_AUTH_SOCK

