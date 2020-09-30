#!/bin/sh
shopt -s extglob

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
    . env3/bin/activate
}

function venv() {
    virtualenv -p python3.7 env3
    activate
    (pip install -r ?(dev_)requirements?(_dev).txt || pip install -r requirements.txt)
}

function venv-rebuild {
    rm -rf venv
    venv
}

function ptp {
    ptpython
}

function git-log-release-format {
    git log --pretty=format:"%Cred%h%Creset %s %Cgreen<%an>%Creset"
}

function tg {
    aws-okta exec mba -- terragrunt "$@" | grep -v "Refreshing state..."
}

function code {
    if [[ -z "$@" ]]
    then
        cd ~/code/flatiron
    else
        cd ~/code/"$@"
    fi
}

function git-oldest-ancestor {
    diff -u <(git rev-list --first-parent $(git rev-parse HEAD)) \
         <(git rev-list --first-parent master) | \
        sed -ne 's/^ //p' | head -1
}

alias e='emacsclient -n'
