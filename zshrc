# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  tmux attach -t default || tmux new -s default
fi

#
#alias python='python -B'
#cworkon(){source ~/CanopyEnvs/$1/bin/activate;} # Alias for workon with canopy environment instead
alias ipythons='ipython --profile=science'
alias grep='grep --color=auto'
alias rmpyc="find . -name '*.pyc' -delete"
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
      alias vim='nvim'
      ;;
   darwin*)
      alias open='open' #make this linux only!
      alias vim='mvim -v'
      #export CXX=/usr/local/cellar/gcc/8.1.0/bin/g++-8
      # put your paths to clang-4.0 and clang++-4.0:
      export CC=/usr/local/opt/llvm/bin/clang
      export CXX=/usr/local/opt/llvm/bin/clang++
      export CXX11=/usr/local/opt/llvm/bin/clang++
      export CXX1X=/usr/local/opt/llvm/bin/clang++
      export PATH="/usr/local/opt/llvm/bin:$PATH"
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

#Default path
#export PATH=/usr/bin:/bin:/usr/sbin:/sbin:.

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
export ZSH_THEME="powerlevel10k/powerlevel10k"
#export ZSH_THEME="agnoster"
#export ZSH_THEME="robbyrussell"
# For gnome set the ls colors to something proper may need changing for osx to check for the file

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
#autojumo you press j Darwin and it will jump to the most likely directory
#extract something.tar.gz will extract pretty much any compression type
#pyclean deletes all .pyc files and pyfind finds all .py files
#osx has a cdf which cd's to the current finders window


#And add runtime and headers
#export LDFLAGS = -L/usr/local/opt/llvm/lib
#export CPPFLAGS = -I/usr/local/opt/llvm/include

#source /usr/local/bin/virtualenvwrapper.sh
source $ZSH/oh-my-zsh.sh

export TEXINPUTS=:$HOME/Work/publications/tex_inputs//
export BSTINPUTS=:$HOME/Work/publications/tex_inputs//
export BIBINPUTS=:$HOME/Work/publications/bib//

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
export PROWLER_IO_HOME=/home/alansaul/PROWLER_IO_HOME

# Fuzzy finding
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="/home/alan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

source ~/envs/prowler2/bin/activate

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
