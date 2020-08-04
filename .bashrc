
bind "set completion-ignore-case on"

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

# GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
# source ~/dotfiles/bash-git-prompt/gitprompt.sh
source ~/dotfiles/bash-basic-git-prompt.sh
