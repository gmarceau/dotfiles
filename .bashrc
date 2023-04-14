[[ -f ~/dotfiles/.bash-env.sh ]] && source ~/dotfiles/.bash-env.sh
[[ -f ~/dotfiles/.bash-aliases.sh ]] && source ~/dotfiles/.bash-aliases.sh

# Bash completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
[[ -f ~/dotfiles/.fzf.bash ]] && source ~/dotfiles/.fzf.bash

function cpu_temp {
    sudo powermetrics --samplers smc |grep -i "CPU die temperature"
}

alias e='emacsclient -n'

source ~/dotfiles/.bash-completions.sh

# GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
# source ~/dotfiles/bash-git-prompt/gitprompt.sh
source ~/dotfiles/bash-basic-git-prompt.sh
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
