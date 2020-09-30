bind "set completion-ignore-case on"
shopt -s histappend
shopt -s extglob
HISTFILESIZE=1000000
HISTCONTROL=ignoredups
HISTIGNORE='ls:bg:fg:history'
HISTTIMEFORMAT='%F %T '


export FZF_DEFAULT_COMMAND='ag -al'
export EDITOR="emacsclient -a 'emacs -nw'"

[[ -f ~/dotfiles/.bash-aliases.sh ]] && source ~/dotfiles/.bash-aliases.sh

# Bash completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
[[ -f ~/dotfiles/.fzf.bash ]] && source ~/dotfiles/.fzf.bash
[[ -f ~/code/phacility/arcanist/support/shell/hooks/bash-completion.sh ]] && \
    source ~/code/phacility/arcanist/support/shell/hooks/bash-completion.sh # arcanist-shell-complete

function cpu_temp {
    sudo powermetrics --samplers smc |grep -i "CPU die temperature"
}

alias e='emacsclient -n'

# GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
# source ~/dotfiles/bash-git-prompt/gitprompt.sh
source ~/dotfiles/bash-basic-git-prompt.sh
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
