# Author: Anish Sevekari
# Last Edited: Fri 24 Feb 2023 03:51:23 PM EST

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set zsh variables and source corresponding settings
ZSH_HOME=${ZDOTDIR:-$HOME/.zsh}
fpath=($ZSH_HOME/functions $ZSH_HOME/completions /run/current-system/sw/share/bash-completion/completions $fpath)

################################################################################
# zinit ########################################################################
################################################################################

# Auto installing zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
if [ ! -d $(dirname $ZINIT_HOME) ]; then
	mkdir -p "$(dirname $ZINIT_HOME)"
	git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi
source "${ZINIT_HOME}/zinit.zsh"

# helper functions
local function __bind_history_keys(){
	bindkey "$terminfo[kcuu1]" history-substring-search-up
	bindkey "$terminfo[kcud1]" history-substring-search-down
	bindkey -M vicmd 'k' history-substring-search-up
	bindkey -M vicmd 'j' history-substring-search-down
}

# plugins
zinit wait lucid light-mode for \
	hlissner/zsh-autopair \
	jeffwalter/zsh-plugin-cd-ssh \
	arzzen/calc.plugin.zsh \
	chisui/zsh-nix-shell \
	laggardkernel/zsh-thefuck \
	MichaelAquilina/zsh-auto-notify

## completions and autosuggestions
zinit wait lucid light-mode for \
		atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20" \
		atload"_zsh_autosuggest_start" \
	zsh-users/zsh-autosuggestions \
		atpull'zinit creinstall -q .' \
		blockf \
	zsh-users/zsh-completions \
  srijanshetty/zsh-pip-completion \
  spwhitt/nix-zsh-completions 


## history searching
zinit wait lucid light-mode for \
		atinit"
			zstyle :history-search-multi-word page-size 10
			zstyle :history-search-multi-word highlight-color fg=red,bold
			zstyle :plugin:history-search-multi-word reset-prompt-protect 1
		" \
	zdharma-continuum/history-search-multi-word \
		atload'__bind_history_keys' \
	zsh-users/zsh-history-substring-search

## syntax highlighting
zinit wait lucid light-mode for \
		atinit"
			typeset -gA FAST_HIGHLIGHT; FAST_HIGHLIGHT[git-cmsg-len]=100;
			ZINIT[COMPINIT_OPTS]=-C;
			zicompinit;
			zicdreplay;
		" \
	zdharma-continuum/fast-syntax-highlighting \
	zlsun/solarized-man \

## Prompt theme
# zinit ice compile'(pure|async).zsh' pick"async.zsh" src"pure.zsh"
# zinit light sindresorhus/pure 
zinit ice depth=1
zinit light romkatv/powerlevel10k

## Navigation
zinit wait lucid light-mode for \
	changyuheng/zsh-interactive-cd \
	# wting/autojump


################################################################################
# Settings #####################################################################
################################################################################

KEYTIMEOUT=1 # keytiming 10ms
CASE_SENSITIVE="false" # case sensative searches
HYPHEN_INSENSITIVE="true" # hyphen insensative searches
unsetopt beep # removes beeps in windows

setopt histreduceblanks # remove blanks
setopt histignorealldups # ignore repeats in history
setopt sharehistory # share history between terminals

setopt correct # spelling correction
COMPLETION_WAITING_DOTS="true"
bindkey -v # Use vim keybindings
# vim keybindings are a problem if you are already in nvim


# History
# Keep 1000000 lines of history within the shell and save it to ~/.zsh_history:
HISTFILE=$ZSH_HOME/zsh_history
HISTSIZE=1000000
HISTFILESIZE=1000000
SAVEHIST=1000

setopt incappendhistory



# zsh/autosuggestions color change
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'



################################################################################
# Keybindings ##################################################################
################################################################################

# Search
bindkey -M vicmd '?' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-forward

# history search keybindings -- reset keys at start of the shell
# history search keys would be bound when plugin loads
bindkey -r "$terminfo[kcuu1]"
bindkey -r "$terminfo[kcud1]"
bindkey -r -M vicmd 'k'
bindkey -r -M vicmd 'j'

################################################################################
# Functions and Commands #######################################################
################################################################################

# Importing functions and commands
for file in $ZSH_HOME/functions/*.zsh; do
	source $file
done

_has() {
  return $( whence $1 >/dev/null )
}
_color() {
  return $( [ -z "$INSIDE_EMACS" ] )
}

# run neofetch at start of ssh session
if [[ -n $SSH_CONNECTION ]]; then
	if _has neofetch; then
		neofetch
	fi
fi

# loading autojump
# autojump is installed using nixos, and it needs to be imported from nix-store.
if _has autojump; then
	AUTOJUMP_BIN=$(readlink $(which autojump))
	AUTOJUMP_DIR=$(dirname $(dirname $AUTOJUMP_BIN))
	AUTOJUMP_SH=$AUTOJUMP_DIR/etc/profile.d/autojump.sh
	[[ -s $AUTOJUMP_SH ]] && source $AUTOJUMP_SH
fi



# FZF config
if _has fzf; then
	if _has ag; then
		export FZF_DEFAULT_COMMAND='ag -g ""' # Use Ag if it exists
		export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
		export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
	fi
  export FZF_DEFAULT_OPTS='
		--height 40% --reverse
  '
fi

################################################################################
# Aliases ######################################################################
################################################################################

# use colored versions of commands by default
if check dircolors; then
	if test -r $ZSH_HOME/dircolors; then
		eval "$(dircolors -b $ZSH_HOME/dircolors)"
	else
		eval "$(dircolors -b)"
	fi
	alias ls='ls --color=auto'
	alias dir='dir --color=auto'
	alias vdir='vdir --color=auto'

	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
#alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(echo $history[$HISTCMD]|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias alert='tput bel'

# Importing Aliases
for file in $ZSH_HOME/aliases/*.zsh; do
	source $file
done

################################################################################
# Definitions ##################################################################
################################################################################

export PAGER='less'
if _has nvim; then
	export EDITOR='nvim'
elif _has vim; then
	export EDITOR='vim'
fi


################################################################################
# Prompt #######################################################################
################################################################################
autoload -U promptinit && promptinit

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f "$ZSH_HOME/p10k.zsh" ]] || source "$ZSH_HOME/p10k.zsh"

################################################################################
# Completions ##################################################################
################################################################################

# Use modern completion system
autoload -Uz compinit bashcompinit
compinit 
bashcompinit

eval "$(register-python-argcomplete pubs)"

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Add local dirs to path -- required for convinience
path+=($HOME/bin)
path+=($HOME/.local/bin)
export PATH

# vim:ft=zsh
