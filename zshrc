#AKIAI36R7HIHP226ECAA", Path to your oh-my-zsh configuration.
export HOME=/Users/john_fan
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="kardan"
#ZSH_THEME="agnoster"
#ZSH_THEME="sporty-256"
#ZSH_THEME="zhann"
 
#ZSH_THEME='powerlevel9k/powerlevel9k'

export EDITOR=/bin/vim
export SVN_EDITOR=/bin/vim

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
#
PATH=.:/usr/local/bin:/bin:/usr/bin:~/bin:/Library/Tex/texbin:/usr/local/sbin:/usr/sbin:/Applications/SageMath:

export PATH

alias   le="less "
alias   ls="/bin/ls -sqCF "
alias   lt="/bin/ls -sqtCF"
alias   c="clear"
alias   h="history"
alias   x="chmod +x"
alias   grep="grep -n "
alias   mv="mv -i"
alias   cp="cp -i"
alias   del="rm -f"
alias   rm="rm -i"
alias   free="free -m"
alias   df="df -m"
alias   pc="xclip -o -selection clipboard | xclip -i "


alias -s tex=vim
alias -s html=w3m
alias -s org=w3m
alias -s com=w3m

alias rr=ranger


ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

#source zsh-syntax-highlighting/zsh-syntax-highlighting.zsh



export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export JAVACMD=$JAVA_HOME/bin/java

unset GREP_OPTIONS

bindkey "^R" history-incremental-search-backward
bindkey -v



# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# # Emacs style
# zle -N edit-command-line
# bindkey '^xe' edit-command-line
# bindkey '^x^e' edit-command-line
# # Vi style:
zle -N edit-command-line
bindkey -M vicmd v edit-command-line


stty ixany 
stty ixoff -ixon

bindkey "^R" history-incremental-search-backward

function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

function ppgrep() {
    if [[ $1 == "" ]]; then
        PERCOL=percol
    else
        PERCOL="percol --query $1"
    fi
    ps aux | eval $PERCOL | awk '{ print $2 }'
}

function ppkill() {
    if [[ $1 =~ "^-" ]]; then
        QUERY=""            # options only
    else
        QUERY=$1            # with a query
        [[ $# > 0 ]] && shift
    fi
    ppgrep $QUERY | xargs kill $*
}


set statusline=%b\ %B

bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

bindkey '^P' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

#powerline-daemon -q
#POWERLINE_ZSH_CONTINUATION=1
#POWERLINE_ZSH_SELECT=1
#. /usr/local/lib/python3.5/dist-packages/powerline/bindings/zsh/powerline.zsh

zmodload zsh/mapfile
zmodload zsh/regex
setopt extended_glob

# zmodload zsh/pcre

autoload -U edit-command-line

zle -N edit-command-line
bindkey -M vicmd v edit-command-line
setopt PUSHD_MINUS

foreground-vi() {
  fg %vim
}
zle -N foreground-vi
bindkey '^Z' foreground-vi
DIRSTACKSIZE=19
DIRSTACKFILE=~/.zdirs
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

autoload -U zmv

_tmux_pane_words() {
  local expl
  local -a w
  if [[ -z "$TMUX_PANE" ]]; then
    _message "not running inside tmux!"
    return 1
  fi
  w=( ${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
  _wanted values expl 'words from current tmux pane' compadd -a w
}

zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^Xt' tmux-pane-words-prefix
bindkey '^X^X' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

export GOPATH=$HOME/go

export MINIO_SECRET_KEY=4j0DiLjchQwxxXqDmA2bZiMRVC693xn81wN5omif
export MINIO_ACCESS_KEY=8UY5G5GWBB2UE44XLR79

export ENDPOINT=9000
export TERM=xterm-256color

export MY_ZONE=us-east-f
let fortran_free_source=1
let fortran_have_tabs=1
let fortran_more_precise=1
let fortran_do_enddo=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(fasd --init auto)"
alias v='f -e vim' # quick opening files with vim
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (files and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)

export AWS_SECRET_ACCESS_KEY=oXOeBS4FPWEPkLsplNIZZYCC34OffFyyueN02AD4
export AWS_ACCESS_KEY_ID=AKIAINZXG6SD4LLTATBA

alias dca='docker ps -a'
alias dra='docker stop $(docker ps -a -q); docker rm $(docker ps -a -q); docker rmi $(docker images -q);di;dc;dca'
alias dri='docker rmi $(docker images -q);di;dc;dca'
alias drc='docker stop $(docker ps -a -q); docker rm $(docker ps -a -q);di;dc;dca'

alias sublime='/opt/sublime_text/sublime_text'

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'


export GOARCH="amd64"
export GOBIN=""
export GOCHAR="6"
export GOEXE=""
export GOHOSTARCH="amd64"
export GOHOSTOS="linux"
export GOOS="linux"
export GORACE=""
export GOROOT="/usr/local/go"
export CC="gcc"
export GOGCCFLAGS="-fPIC -m64 -pthread -fmessage-length=0"
export CXX="g++"
export CGO_ENABLED="1"


# Syntax highlighting and tab completion

export TERM=xterm-256color

#POWERLEVEL9K_MODE='awesome-fontconfig'
#POWERLEVEL9K_MODE='awesome-patched'
POWERLEVEL9K_MODE='nerdfont-complete'
source ~/powerlevel9k/powerlevel9k.zsh-theme

# Prompt settings
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_RPROMPT_ON_NEWLINE=true
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%K{white}%k"
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%K{green}%F{black} \uf155 %f%F{green}%k\ue0b0%f "

# Separators
POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR=$'\ue0b0'
POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR=$'\ue0b1'
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR=$'\ue0b2'
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR=$'\ue0b7'

# Dir colours
POWERLEVEL9K_DIR_HOME_BACKGROUND='black'
POWERLEVEL9K_DIR_HOME_FOREGROUND='white'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND='black'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND='white'
POWERLEVEL9K_DIR_DEFAULT_BACKGROUND='yellow'
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND='black'
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

# OS segment
POWERLEVEL9K_OS_ICON_BACKGROUND='black'
POWERLEVEL9K_LINUX_ICON='%F{cyan} \uf303 %F{white} arch %F{cyan}linux%f'

# VCS icons
POWERLEVEL9K_VCS_GIT_ICON=$'\uf1d2 '
POWERLEVEL9K_VCS_GIT_GITHUB_ICON=$'\uf113 '
POWERLEVEL9K_VCS_GIT_GITLAB_ICON=$'\uf296 '
POWERLEVEL9K_VCS_BRANCH_ICON=$''
POWERLEVEL9K_VCS_STAGED_ICON=$'\uf055'
POWERLEVEL9K_VCS_UNSTAGED_ICON=$'\uf421'
POWERLEVEL9K_VCS_UNTRACKED_ICON=$'\uf00d'
POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON=$'\uf0ab '
POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON=$'\uf0aa '

# VCS colours
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='blue'
POWERLEVEL9K_VCS_MODIFIED_FOREGROUND='black'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='green'
POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND='black'
POWERLEVEL9K_VCS_CLEAN_BACKGROUND='green'
POWERLEVEL9K_VCS_CLEAN_FOREGROUND='black'

# VCS CONFIG
POWERLEVEL9K_SHOW_CHANGESET=false


# Status
POWERLEVEL9K_OK_ICON=$'\uf164'
POWERLEVEL9K_FAIL_ICON=$'\uf165'
POWERLEVEL9K_CARRIAGE_RETURN_ICON=$'\uf165'

# Battery
POWERLEVEL9K_BATTERY_LOW_FOREGROUND='red'
POWERLEVEL9K_BATTERY_CHARGING_FOREGROUND='blue'
POWERLEVEL9K_BATTERY_CHARGED_FOREGROUND='green'
POWERLEVEL9K_BATTERY_DISCONNECTED_FOREGROUND='blue'
POWERLEVEL9K_BATTERY_VERBOSE=false

# Time
POWERLEVEL9K_TIME_FORMAT="%F{black}%D{%I:%M}%f"
POWERLEVEL9K_TIME_BACKGROUND='blue'


#Icon config
POWERLEVEL9K_HOME_ICON='\UF20E'
POWERLEVEL9K_SUB_ICON='\UF07C'
POWERLEVEL9K_FOLDER_ICON='\UF07B'
#POWERLEVEL9K_STATUS_OK_ICON='\UF2B0'
POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR='\UE0BC'

# User with skull
user_with_skull() {
    echo -n "\ufb8a $(whoami)"
}
POWERLEVEL9K_CUSTOM_USER="user_with_skull"

# Command auto-correction.
ENABLE_CORRECTION="true"

# Command execution time stamp shown in the history command output.
HIST_STAMPS="mm/dd/yyyy"

# Plugins to load
plugins=(git virtualenv)
source $ZSH/oh-my-zsh.sh

# Prompt elements
#POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_user dir vcs)
#POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs time battery)

Plugins=(git z)
#source ~/.fonts/*.sh
#source ~/bin/icons.zsh

set -o vi
alias v='f -e vim'
export PATH=/usr/local/opt/curl/bin:/usr/local/go/bin:$PATH

#export PATH="~/.pyenv/bin:$PATH"
#eval "$(pyenv init -)"

export PATH="/usr/local/opt/krb5/bin:$PATH"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="/usr/local/opt/krb5/bin:$PATH"
