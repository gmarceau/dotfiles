
bind "set completion-ignore-case on"
shopt -s histappend
HISTFILESIZE=1000000
HISTCONTROL=ignoredups
HISTIGNORE='ls:bg:fg:history'
HISTTIMEFORMAT='%F %T '


export FZF_DEFAULT_COMMAND='ag -al'
export PATH=$HOME/bin:$PATH
export EDITOR="emacsclient -ca 'emacs -nw'"

function ff {
    find . -name $1
}

function ffl {
    find . -iname \*$1\*
}

function ffe {
    find . -name \*.$1
}

function wt {
    watch -n 1 $*
}

function pr {
    hub pull-request | grep https://github | \
        xclip -selection c && \
        echo `xclip -o -selection c` copied to clipboard
}

function activate {
    . venv/bin/activate
}

function venv() {
    virtualenv -p python3.7 venv
    activate
    (pip install -r requirements_dev.txt || pip install -r requirements.txt)
}

function venv-rebuild {
    rm -rf venv
    venv
}

function ptp {
    ptpython
}

alias e='emacsclient -n'

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
[[ -f ~/dotfiles/.fzf.bash ]] && source ~/dotfiles/.fzf.bash


# GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
# source ~/dotfiles/bash-git-prompt/gitprompt.sh
source ~/dotfiles/bash-basic-git-prompt.sh
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
