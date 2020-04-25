#alias python='python -B'
#cworkon(){source ~/CanopyEnvs/$1/bin/activate;} # Alias for workon with canopy environment instead
alias ipythons='ipython --profile=science'
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
      alias vim='mvim -v'
      ;;
esac

notes() {
    local fpath=$HOME/notes.md
    if [ "$1" == "gvim" ]; then
        gvim + $fpath
    elif [ "$1" == "vim" ]; then
        gvim + $fpath
    elif [ "$1" == "date" ]; then
        echo '' >> $fpath
        echo '# '`date +"%m-%d-%Y-%T"` >> $fpath
        echo '---------------------' >> $fpath
    elif [ "$1" == "" ]; then
        less +G $fpath
    else
        echo '' >> $fpath
        echo $@ >> $fpath
    fi
}

#function detexcomment { cat "$1" | sed '/\\begin{comment}/,/\\end{comment}/d' | detex | wc; }

# Bacward search in the shell history with <C-r>
export EDITOR="vim"
#set editing-mode vi
bindkey -v 
bindkey '\e[3~' delete-char
bindkey '^R' history-incremental-search-backward
setopt hist_ignore_all_dups

# Locks to first column, like after Ctrl-Cing a program http://jonisalonen.com/2012/your-bash-prompt-needs-this/
#$PS1="\[\033[G\]$PS1"

export BIBINPUTS=~/Work/BibTeX:~/Work/BibTeX/bib

export WORKON_HOME=~/Envs

#export AWT_TOOLKIT=MToolkit
export MATLAB_JAVA=/usr/lib/jvm/default-java/jre
#_JAVA_AWT_WM_NONREPARENTING=1; export _JAVA_AWT_WM_NONREPARENTING

export PYTHONPATH="${PYTHONPATH}"

#Default path
PATH=/usr/bin:/bin:/usr/sbin:/sbin:.

#Force python to 32 bit
#export VERSIONER_PYTHON_PREFER_32_BIT=yes

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#export ZSH_THEME="blinks-dark-bg"
#export ZSH_CUSTOM="$HOME/Work/Code/dotfiles/zsh_custom:$HOME/dotfiles/zsh_custom"
#export ZSH_CUSTOM="$HOME/Work/Code/dotfiles/zsh_custom"
#export ZSH_THEME="blinks-solarized"
export ZSH_THEME="blinks"
#export ZSH_THEME="robbyrussell"

# For gnome set the ls colors to something proper may need changing for osx to check for the file
# Color listing
if type dircolors > /dev/null; then
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
plugins=(git github osx vi-mode python autojump colored-man extract)
#autojumo you press j Darwin and it will jump to the most likely directory
#extract something.tar.gz will extract pretty much any compression type
#pyclean deletes all .pyc files and pyfind finds all .py files
#osx has a cdf which cd's to the current finders window

#Autojump pluggin needs patching for macports with this in the autojump file:
export FPATH="$FPATH:/opt/local/share/zsh/site-functions/"
if [ -f /opt/local/etc/profile.d/autojump.sh ]; then
    . /opt/local/etc/profile.d/autojump.sh
fi

PATH="/home/alans/matlab/bin:/usr/local/bin:/opt/local/lib/postgresql93/bin:/usr/local/sbin:/usr/local/mysql/bin:/Users/alansaul/sandbox:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:$PATH"

# added by Anaconda 2.0.1 installer
export PATH="/Users/alansaul/Work/Code/main/Platform_Generic/learning/learning_prototype:/Library/TeX/texbin:/Users/alansaul/Work/Code/AirConnect/bin:/usr/local/anaconda3/bin:/home/alans/anaconda/bin:~/Work/Code/eclipse-workspace/GarminSDKs/connectiq-sdk-mac-2.4.2:$PATH:"

#export CXX=/usr/local/cellar/gcc/8.1.0/bin/g++-8
# put your paths to clang-4.0 and clang++-4.0:
export CC=/usr/local/opt/llvm/bin/clang
export CXX=/usr/local/opt/llvm/bin/clang++
export CXX11=/usr/local/opt/llvm/bin/clang++
export CXX1X=/usr/local/opt/llvm/bin/clang++

#And add runtime and headers
#export LDFLAGS = -L/usr/local/opt/llvm/lib
#export CPPFLAGS = -I/usr/local/opt/llvm/include

#source /usr/local/bin/virtualenvwrapper.sh
source $ZSH/oh-my-zsh.sh

# Added by Canopy installer on 2014-07-30
# VIRTUAL_ENV_DISABLE_PROMPT can be set to '' to make bashprompt show that Canopy is active, otherwise 1
#VIRTUAL_ENV_DISABLE_PROMPT=1 source /home/alans/Enthought/Canopy_64bit/User/bin/activate

export TEXINPUTS=:$HOME/Work/publications/tex_inputs//
export BSTINPUTS=:$HOME/Work/publications/tex_inputs//
export BIBINPUTS=:$HOME/Work/publications/bib//

__conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/alansaul/miniconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/Users/alansaul/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/alansaul/miniconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/Users/alansaul/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup

export SLUGIFY_USES_TEXT_UNIDECODE=yes
export PROWLER_IO_HOME=/Users/alansaul/prowler_home

# Fuzzy finding
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
