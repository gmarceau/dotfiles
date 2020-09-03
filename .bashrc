
bind "set completion-ignore-case on"
shopt -s histappend
HISTFILESIZE=1000000
HISTCONTROL=ignoredups
HISTIGNORE='ls:bg:fg:history'
HISTTIMEFORMAT='%F %T '


export FZF_DEFAULT_COMMAND='ag -al'
export PATH=$HOME/bin:$PATH
export EDITOR="emacsclient -ca 'emacs -nw'"

source ~/dotfiles/.bash-aliases.sh

# Bash completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
[[ -f ~/dotfiles/.fzf.bash ]] && source ~/dotfiles/.fzf.bash
[[ -f ~/code/phacility/arcanist/support/shell/hooks/bash-completion.sh ]] && \
    source ~/code/phacility/arcanist/support/shell/hooks/bash-completion.sh # arcanist-shell-complete

# GIT_PROMPT_THEME_FILE=~/dotfiles/git_prompt_theme_file
# source ~/dotfiles/bash-git-prompt/gitprompt.sh
source ~/dotfiles/bash-basic-git-prompt.sh
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
