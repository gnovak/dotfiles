
PS1='[\u@\h \w]$ '
PATH=~/bin:~/bin/local/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/cuda/bin:$PATH
MANPATH=/opt/local/man:~/bin/man:$MANPATH

alias cp='cp -i'	
alias mv='mv -i'
alias rm='rm -i'		
alias du='du -k'				
alias ls='/bin/ls -hFG'

export EDITOR='emacs -nw --no-init'
export IP=`~/bin/ip-address`
export RSYNC_RSH=ssh
export CVSROOT=novak@ssh.ucolick.org:/home/public/novak/bin/cvs
export CVS_RSH=ssh
export CVSEDITOR=pico
export TEXINPUTS=$HOME/bin/tex//:
export BSTINPUTS=$HOME/bin/tex//:
export BIBINPUTS=$HOME/Papers/bib:
# For pyfits
export NUMERIX=numpy  
# clozure CL
export CCL_DEFAULT_DIRECTORY=/usr/local/ccl
# Advice from Macports for building qt3 software
export QTDIR=/opt/local/lib/qt3
# export PYTHONPATH=./gsnpy:/Users/novak/bin/local/lib/python2.5/site-packages/

# Search path for info files.  If INFOPATH ends with a colon, then
# Emacs searches both INFOPATH and Info-default-directory-list
#export INFOPATH=
#export PYTHONDOCS=/sw/share/doc/python23/html/
#export GS_LIB=~/bin/gs/fonts
#export BSTINPUTS=$HOME/bin/tex//:
#export PERL5LIB=~/Pictures/Web/perl:~/bin/perl/lib/perl5/site_perl/:$PERL5LIB
#export SBCL_HOME=~/bin/root/lib/sbcl

# Enable bash completion
if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

##############################
# IDL Stuff
if [ -n "$IP" ]; then 
    if [ $IP == "192.168.9.250" ]; then 
        export LM_LICENSE_FILE=1700@mambo
    else
        export LM_LICENSE_FILE=1700@localhost
    fi
fi
export IDL_VERSION=${IDL_VERSION:-6.3}
# These next two are needed for running IDL inside Python
# They're also set in .emacs
export DYLD_LIBRARY_PATH=/Applications/rsi/idl_$IDL_VERSION/bin/bin.darwin.ppc
export XPPATH="/Applications/rsi/idl_$IDL_VERSION/resource/xprinter"
export IDL_STARTUP=/Users/novak/bin/idl-pros/startup.pro
export IDL_PATH="+${HOME}/bin/idl-pros:+${HOME}/Projects/Thesis/idl:<IDL_DEFAULT>"
export IDL_DIR=/Applications/rsi/idl_$IDL_VERSION

##############################

# Start Dropbox if it isn't already running.
# Dropbox itself checks for this, so this test just avoids an error message.
if [ -x ~/.dropbox-dist/dropbox ]; then 
    if [ -z "`ps aux | grep -f ~/.dbox-name`" ]; then 
        ~/.dropbox-dist/dropbox &
    fi
fi

