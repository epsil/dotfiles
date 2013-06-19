# Set up the prompt

export PATH=$PATH:$HOME/.cabal/bin
export EDITOR=vim
export TERM=xterm-256color

autoload +XU colors
autoload -Uz promptinit
promptinit
prompt suse

setopt histignorealldups histignorespace sharehistory
setopt bash_auto_list list_ambiguous
setopt extendedglob

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Disable XON/XOFF flow control
stty -ixon

# Keep 10,000 lines of history within the shell and save it to ~/.bash_history:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.bash_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# %n = username, %m = host, %~ = directory, %# = terminator
# PS1=$'%{\e[01;32m%}%n@%m%{\e[0m%}:%{\e[01;34m%}%~%{\e[0m%}%# '
# bash-like prompt: $ instead of %
PS1=$'%{\e[01;32m%}%n@%m%{\e[0m%}:%{\e[01;34m%}%~%{\e[0m%}$ '

# command-not-found (overrides preexec and precmd)
. /etc/zsh_command_not_found

# Ctrl-X Ctrl-E
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line
