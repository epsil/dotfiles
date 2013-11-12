export PATH=/local/X11/bin/:/local/bin:$PATH:$HOME/.cabal/bin:/hom/inf3151/tools/bin:/usr/local/cuda/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/
export LANG=nb_NO.UTF-8
export LC_ALL=nb_NO.UTF-8
export TERM=xterm-256color
export EDITOR=vi

# Set up the prompt

export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# export PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
