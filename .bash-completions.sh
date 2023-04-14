#!/bin/sh

function _code_alias_completion()
{
    WORDS=$(ls ~/code)
    COMPREPLY=($(compgen -W "$WORDS" -- "${COMP_WORDS[${COMP_CWORD}]}"))
}


complete -F _code_alias_completion code

source ~/code/hcdp-spark/bin/tab-completions

source ~/dotfiles/bash-completions/poetry
