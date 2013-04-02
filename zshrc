#alias python='python -B'
alias sshdcs='ssh acp12ads@stulogin.dcs.shef.ac.uk'
alias ipythons='ipython --profile=science'
alias cdblog='cd ~/Work/Code/Websites/RailsBlog/'
alias bx='bundle exec'
alias start-eclimd='/Applications/eclipse/eclimd'
alias grep='grep --color=auto#'
#Alias for zsh plugin which does safe deletion
alias rm=trash

export LS_OPTIONS='--color'
alias l='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -lh'
alias lll='ls $LS_OPTIONS -alh'
alias sl='ls $LS_OPTIONS' # often screw this up

case "$OSTYPE" in
   cygwin*)
      alias uopen="cmd /c start"
      ;;
   linux*)
      alias uopen='xdg-open' #make this linux only!
      ;;
   darwin*)
      alias uopen='open' #make this linux only!
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

export DYLD_LIBRARY_PATH=/bin:/Applications/weka-3-6-3/:/Applications/weka-3-6-3/weka.jar

export CLASSPATH=.:/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Classes/classes.jar:/Users/alansaul/Documents/workspace/Jogl/lib/gluegen-rt.jar:/Users/alansaul/Documents/workspace/Jogl/lib/jogl.jar:/Applications/weka-3-6-3/weka.jar

#For NPM modules
export NODE_PATH='/usr/local/lib/node_modules'

#Default path
PATH=/usr/bin:/bin:/usr/sbin:/sbin:.

#Setup RVM (ruby version manager)
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

export PYTHONPATH=/Library/Python/2.6/site-packages:/home/alans/Work/GPy:/home/alans/Work:/opt/graphite/webapp:/opt/graphite/whisper:/Users/alansaul/Work/CompSci/Darwin/Code/darwinproject2011/DarwinTools:/Users/alansaul/Work/CompSci/Darwin/Code:/Users/alansaul/Work/Code/OpenCV/trunk:"${PYTHONPATH}"

#Force python to 32 bit
export VERSIONER_PYTHON_PREFER_32_BIT=yes

# Path to your oh-my-zsh configuration.
ZSH=$HOME/dotfiles/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#export ZSH_THEME="blinks-dark-bg"
export ZSH_THEME="blinks"
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
plugins=(rails3 git ruby macports gem github osx rvm vi-mode python autojump)
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
export PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:/Users/alansaul/.rvm/gems/ruby-1.9.2-p180/bin:/Users/alansaul/.rvm/gems/ruby-1.9.2-p180@global/bin:/Users/alansaul/.rvm/rubies/ruby-1.9.2-p180/bin:/Users/alansaul/.rvm/bin:/usr/local/share/npm/bin:/opt/node/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/sw/bin:/sw/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/X11R6/bin:/Users/alansaul/Documents/MATLAB/Dissertation_Code_WC:/Users/alansaul/sandbox:/usr/src/play-1.2:/Applications/Autodesk/maya2011/Maya.app/Contents/bin/:/Users/alansaul/Work/Code/OpenCV/trunk:/Users/alansaul/.gem/ruby/1.8/bin:/usr/bin:/bin:/usr/sbin:/sbin:.

source $ZSH/oh-my-zsh.sh


#No longer necessary for zsh, osx plugin has a nicer trash implementation
#safe_rm() # Safe rm procedure
#{
    ## Cycle through each argument for deletion
    #for file in $*; do
        #if [ -e $file ]; then
            ## Target exists and can be moved to Trash safely
            #if [ ! -e ~/.Trash/$file ]; then
                #mv $file ~/.Trash
                ## Target exists and conflicts with target in Trash
            #elif [ -e ~/.Trash/$file ]; then
                ## Increment target name until 
                ## there is no longer a conflict
                #i=1
                #while [ -e ~/.Trash/$file.$i ];
                #do
                    #i=$(($i + 1))
                #done

                ## Move to the Trash with non-conflicting name
                #mv $file ~/.Trash/$file.$i
            #fi

            ## Target doesn't exist, return error
        #else
            #echo "rm: $file: No such file or directory";
        #fi
    #done
#}
