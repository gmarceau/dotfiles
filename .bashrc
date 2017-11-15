
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

function activate {
    . venv/bin/activate
}

alias e='emacsclient -n'

GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
source ~/dotfiles/bash-git-prompt/gitprompt.sh
