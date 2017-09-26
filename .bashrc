
bind "set completion-ignore-case on"

export PATH=$HOME/bin:$PATH
export EDITOR="emacsclient -ca"

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
