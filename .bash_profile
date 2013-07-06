export LANG=nb_NO.UTF-8
export LC_ALL=nb_NO.UTF-8
export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# export PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
