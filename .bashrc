

export PATH=$HOME/bin:$PATH
export EDITOR="emacsclient -c"

function ff {
    find . -name $1
}

function ffl {
    find . -name \*$1\*
}

function ffe {
    find . -name \*.$1
}

alias e='emacsclient -n'
