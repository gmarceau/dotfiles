function realias() {
    source ~/dotfiles/.zsh-alias.sh
}

function venv() {
    python3.13 -m venv venv && . venv/bin/activate
}

function ffl() {
    find . -name \*${1}\*
}

function activate() {
    . {.venv,venv}/bin/activate
}

function jump() {
    if [[ -z "${1}" ]]; then
        cd $JUMP_CODE_HOME
        return
    fi

    START=$(pwd)
    cd $JUMP_CODE_HOME
    setopt nonomatch
    TARGET=$(fd --type d --full-path --max-depth 4 --exclude docs -F ${1%/} $JUMP_CODE_HOME | head -n 1)
    if [[ -z "${TARGET}" ]]; then
        echo Not found
        cd "${START}"
        return
    fi
    cd ${TARGET}
    setopt nomatch
}

function grbpr() {
    # "git rebase for a pull request"
    git fetch --all
    git rebase -i $(git merge-base origin/master HEAD)
}


function new-mr() {
    glab mr create --fill  --web
}


function smolvm-ls() {
    smolvm machine ls --json |  yq -r '.[]? | .name'
}

# Run a command forcing it to output color, then `less` interpreting the colors
function less-r () {
    unbuffer $* |less -R
}

function cpu_temp {
    sudo powermetrics --samplers smc |grep -i "CPU die temperature"
}

function sparkling {
    mosh sparkling
}
