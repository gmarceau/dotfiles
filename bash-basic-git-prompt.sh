COLOR_GIT_CLEAN='\[\033[1;30m\]'
COLOR_GIT_MODIFIED='\[\033[0;33m\]'
COLOR_GIT_STAGED='\[\033[0;36m\]'
COLOR_RESET='\[\033[0m\]'
BoldYellow='\[\033[1;33m\]'
Yellow='\[\033[0;33m\]'
BoldGreen='\[\033[1;32m\]'
ResetColor='\[\033[0;0m\]'
BoldBlue='\[\033[1;34m\]'
Blue='\[\033[0;34m\]'
Time12a='$(date +%H:%M)'

function git_prompt() {
    if [ -e ".git" ] || [ -e "../.git" ] || [ -e "../../.git" ] || [ -e "../../../.git" ] \
           || [ -e "../../../../.git" ] || [ -e "../../../../../.git" ] || [ -e "../../../../../../.git" ]; then
    branch_name=$(git symbolic-ref -q HEAD)
    branch_name=${branch_name##refs/heads/}
    branch_name=${branch_name:-HEAD}

    echo -n "→ "
    echo -n "${Yellow}$branch_name$COLOR_RESET"

    # if [[ $(git status 2> /dev/null | tail -n1) = *"nothing to commit"* ]]; then
    #   echo -n "$COLOR_GIT_CLEAN$branch_name$COLOR_RESET"
    # elif [[ $(git status 2> /dev/null | head -n5) = *"Changes to be committed"* ]]; then
    #   echo -n "$COLOR_GIT_STAGED$branch_name$COLOR_RESET"
    # else
    #   echo -n "$COLOR_GIT_MODIFIED$branch_name*$COLOR_RESET"
    # fi

    echo -n " "
  fi
}

function prompt() {
    if [ $VIRTUAL_ENV ]; then
        local VENV="($(basename $VIRTUAL_ENV)) "
    fi
    PS1="${BoldBlue}${VENV}${ResetColor}[ ${Yellow}\${?} ${BoldGreen}-- ${Time12a} ${BoldBlue}\w${ResetColor} $(git_prompt)] "
    unset VENV
}

PROMPT_COMMAND=prompt
