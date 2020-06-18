if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
    exec tmux new-session -A -s default
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

alias grep='grep --color=auto'
#Alias for zsh plugin which does safe deletion
alias rm=trash
alias lll='ls $LS_OPTIONS -alh'
alias sl='ls $LS_OPTIONS' # often screw this up
alias j='z'

# vim alias order
if [ $(command -v nvim) ]; then
    export VIM_COMMAND='nvim'
elif [ $(command -v mvim) ]; then
    export VIM_COMMAND='mvim -v'
else
    export VIM_COMMAND='vim'
fi
alias vim=$VIM_COMMAND
export EDITOR=$VIM_COMMAND
export VISUAL=$VIM_COMMAND

case "$OSTYPE" in
    cygwin*)
        alias open="cmd /c start"
        ;;
    linux*)
        alias open='xdg-open' #make this linux only!
        ;;
    darwin*)
        alias open='open' #make this linux only!
        #export CXX=/usr/local/cellar/gcc/8.1.0/bin/g++-8
        # put your paths to clang-4.0 and clang++-4.0:
        export CC=/usr/local/opt/llvm/bin/clang
        export CXX=/usr/local/opt/llvm/bin/clang++
        export CXX11=/usr/local/opt/llvm/bin/clang++
        export CXX1X=/usr/local/opt/llvm/bin/clang++
        export PATH="/usr/local/opt/llvm/bin:$PATH"
esac

export PROWLER_IO_HOME=~/PROWLER_IO_HOME
export VIRTUAL_ENV='prowler2'

#Default path
#export PATH=/usr/bin:/bin:/usr/sbin:/sbin:.
export PYTHONPATH=/home/alan/Code/main/Platform_Generic/tensorflow_ops:$PYTHONPATH
export LD_LIBRARY_PATH=/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH
export LD_INCLUDE_PATH=:/usr/local/cuda/include:/usr/local/cuda/extras/CUPTI/include:$LD_INCLUDE_PATH

#export TEXINPUTS=:$HOME/Work/publications/tex_inputs//
#export BSTINPUTS=:$HOME/Work/publications/tex_inputs//
#export BIBINPUTS=:$HOME/Work/publications/bib//

# Locks to first column, like after Ctrl-Cing a program http://jonisalonen.com/2012/your-bash-prompt-needs-this/
#$PS1="\[\033[G\]$PS1"

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
if [[ -f ~/.p10k.zsh ]]; then 
    export ZSH_THEME="powerlevel10k/powerlevel10k"
else 
    export ZSH_THEME="blinks"
    echo "powerlevel10k not installed"
fi
POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

export SOLARIZED_THEME='dark'

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
plugins=(git github osx vi-mode python z colored-man-pages extract virtualenv)
#z you press j (or z via alias) "word" and it will jump to the most likely directory
#extract something.tar.gz will extract pretty much any compression type
#pyclean deletes all .pyc files and pyfind finds all .py files
#osx has a cdf which cd's to the current finders window

#And add runtime and headers
#export LDFLAGS = -L/usr/local/opt/llvm/lib
#export CPPFLAGS = -I/usr/local/opt/llvm/include

#source /usr/local/bin/virtualenvwrapper.sh
source $ZSH/oh-my-zsh.sh

#__conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/alansaul/miniconda3/bin/conda' shell.bash hook 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    \eval "$__conda_setup"
#else
#    if [ -f "/Users/alansaul/miniconda3/etc/profile.d/conda.sh" ]; then
#        . "/Users/alansaul/miniconda3/etc/profile.d/conda.sh"
#        CONDA_CHANGEPS1=false conda activate base
#    else
#        \export PATH="/Users/alansaul/miniconda3/bin:$PATH"
#    fi
#fi
#unset __conda_setup

export SLUGIFY_USES_TEXT_UNIDECODE=yes

# Fuzzy finding
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
    source "${VIRTUAL_ENV}/bin/activate"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
if [[ -f ~/.p10k.zsh ]]; then 
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
fi
