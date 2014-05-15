#alias python='python -B'
alias sshdcs='ssh acp12ads@stulogin.dcs.shef.ac.uk'
alias ipythons='ipython --profile=science'
alias cdblog='cd ~/Work/Code/Websites/RailsBlog/'
alias bx='bundle exec'
alias start-eclimd='/Applications/eclipse/eclimd'
alias grep='grep --color=auto'
alias rmpyc="find . -name '*.pyc' -delete"
alias matlabt="matlab -nodesktop -nosplash"
#Alias for zsh plugin which does safe deletion
alias rm=trash

export LS_OPTIONS='--color'
alias l='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -lh'
alias lll='ls $LS_OPTIONS -alh'
alias sl='ls $LS_OPTIONS' # often screw this up

case "$OSTYPE" in
   cygwin*)
      alias open="cmd /c start"
      ;;
   linux*)
      alias open='xdg-open' #make this linux only!
      ;;
   darwin*)
      alias open='open' #make this linux only!
      ;;
esac

function detexcomment { cat "$1" | sed '/\\begin{comment}/,/\\end{comment}/d' | detex | wc; }

# Bacward search in the shell history with <C-r>
export EDITOR="vim"
set editing-mode vi
bindkey -v 
bindkey '\e[3~' delete-char
bindkey '^R' history-incremental-search-backward
setopt hist_ignore_all_dups

# Locks to first column, like after Ctrl-Cing a program http://jonisalonen.com/2012/your-bash-prompt-needs-this/
#$PS1="\[\033[G\]$PS1"

export WORKON_HOME=~/Envs
source /usr/local/bin/virtualenvwrapper.sh

export CLASSPATH=.:/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Classes/classes.jar:/Users/alansaul/Documents/workspace/Jogl/lib/gluegen-rt.jar:/Users/alansaul/Documents/workspace/Jogl/lib/jogl.jar:/Applications/weka-3-6-3/weka.jar

#export AWT_TOOLKIT=MToolkit
export MATLAB_JAVA=/usr/lib/jvm/default-java/jre
#_JAVA_AWT_WM_NONREPARENTING=1; export _JAVA_AWT_WM_NONREPARENTING

#For NPM modules
export NODE_PATH='/usr/local/lib/node_modules'

export PYTHONPATH=/Library/Python/2.6/site-packages:/home/alans/Work/GPy:/Users/alansaul/Work/CompSci/PhD:/home/alans/Work:/home/alans/Work:/opt/graphite/webapp:/opt/graphite/whisper:"${PYTHONPATH}"

#Force python to 32 bit
export VERSIONER_PYTHON_PREFER_32_BIT=yes

# Path to your oh-my-zsh configuration.
ZSH=$HOME/dotfiles/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#export ZSH_THEME="blinks-dark-bg"
export ZSH_THEME="blinks-solarized"
#export ZSH_THEME="robbyrussell"

# For gnome set the ls colors to something proper may need changing for osx to check for the file
# Color listing
if [[ -e ~/.dircolors ]]; then
    eval $(dircolors ~/.dircolors)
    zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
fi

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want disable red dots displayed while waiting for completion
# DISABLE_COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby macports gem github osx vi-mode python autojump colored-man extract)
#autojumo you press j Darwin and it will jump to the most likely directory
#extract something.tar.gz will extract pretty much any compression type
#pyclean deletes all .pyc files and pyfind finds all .py files
#osx has a cdf which cd's to the current finders window

#Autojump pluggin needs patching for macports with this in the autojump file:
export FPATH="$FPATH:/opt/local/share/zsh/site-functions/"
if [ -f /opt/local/etc/profile.d/autojump.sh ]; then
    . /opt/local/etc/profile.d/autojump.sh
fi

# MacPorts Installer addition on 2010-11-29_at_00:59:36: adding an appropriate PATH variable for use with MacPorts.
# Customize to your needs...
#
export PATH=$WORKON_HOME:~/dotfiles:/home/alans/Other_Applications/bin:/opt/intel/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/X11R6/bin:/usr/bin:/bin:/usr/sbin:/sbin:.

source $ZSH/oh-my-zsh.sh
