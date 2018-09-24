# Make bash prompt put the host name in the tab title on OS X
# Also modify long-standing bash prompt to remove username.  When do I ever need that?
# \[ .. \] encloses non-printable chars so tools can calculate prompt width
# \e = ascii/ansi escape character (27 decimal, 33 octal, 1b hex)
# ]0; - set tab and window
# ]1; - set tab title (icon name)
# ]2; - set window title (window name)
# \a = ascii/ansi bell, seems to terminate command

# Enable bash completion
if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi

PS1='\[\e]1;\h\a\][\h:\w$(__git_ps1 " (%s)")]$ '

alias cp='cp -i'	
alias mv='mv -i'
alias rm='rm -i'		
alias du='du -k'				
alias ls='/bin/ls -hFG'

# Via patrik
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind 'Control-p: history-search-backward'
bind 'Control-v: history-search-forward'

export EDITOR='emacs -nw --no-init'
export TEXINPUTS=$HOME/bin/tex//:
export MFINPUTS=$HOME/bin/tex//:
export BSTINPUTS=$HOME/bin/tex//:
export BIBINPUTS=$HOME/Documents/Bibtex:
# Pyspark in local mode requires java 8, not java 10
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre

# Git prompt stuff
export GIT_PS1_SHOWDIRTYSTATE="true"
export GIT_PS1_SHOWSTASHSTATE="true"
export GIT_PS1_SHOWUNTRACKEDFILES="true"
export GIT_PS1_SHOWUPSTREAM="auto"

# Set up pyenv
eval "$(pyenv init -)"

PATH=~/bin:~/code/toolbag/bin:/usr/local/texlive/2018/bin/x86_64-darwin:$PATH

# Source local file if it exists
if [ -e ~/.bashrc_local ]; then
   . ~/.bashrc_local
fi
