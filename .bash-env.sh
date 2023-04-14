bind "set completion-ignore-case on"
shopt -s histappend
shopt -s extglob
HISTFILESIZE=1000000
HISTCONTROL=ignoredups
HISTIGNORE='ls:bg:fg:history'
HISTTIMEFORMAT='%F %T '


export FZF_DEFAULT_COMMAND='ag -al'
export EDITOR="emacsclient -a 'emacs -nw'"
export PATH="/usr/local/opt/openjdk@8/bin:$PATH"   # for Spark on EMR development
