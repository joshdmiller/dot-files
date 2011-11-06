# shell options
shopt -s cdspell # correct spelling when changing directories
shopt -s checkwinsize # updates LINES and COLUMNS after each command
shopt -s cmdhist # multiline cmd stored in history as one line
shopt -s dirspell
#shopt -s histappend # after each session, append to $HISTFILE rather than replace
shopt -s no_empty_cmd_completion # don't complete empty lines
#shopt -s nocaseglob # atch filenames case-insensitively

# don't show the same command in history more than once consecutively
export HISTCONTROL=ignoredups


# Check for an interactive session
[ -z "$PS1" ] && return

test -r .dircolors && eval "export $(dircolors -b ~/.bash/dircolors)"
source "$HOME"/.bash/bash_funcs
source "$HOME"/.bash/alias
source "$HOME"/.bash/completion

# set color variables {{{
fgblk="$(tput setaf 0)"     # Black - Regular
fgred="$(tput setaf 1)"     # Red
fggrn="$(tput setaf 2)"     # Green
fgylw="$(tput setaf 3)"     # Yellow
fgblu="$(tput setaf 4)"     # Blue
fgpur="$(tput setaf 5)"     # Purple
fgcyn="$(tput setaf 6)"     # Cyan
fgwht="$(tput setaf 7)"     # White
bfgblk="$(tput setaf 8)"    # Black - Intense
bfgred="$(tput setaf 9)"    # Red
bfggrn="$(tput setaf 10)"   # Green
bfgylw="$(tput setaf 11)"   # Yellow
bfgblu="$(tput setaf 12)"   # Blue
bfgpur="$(tput setaf 13)"   # Purple
bfgcyn="$(tput setaf 14)"   # Cyan
bfgwht="$(tput setaf 15)"   # White
bgblk="$(tput setab 0)"     # Black - Background
bgred="$(tput setab 1)"     # Red
bggrn="$(tput setab 2)"     # Green
bgylw="$(tput setab 3)"     # Yellow
bgblu="$(tput setab 4)"     # Blue
bgpur="$(tput setab 5)"     # Purple
bgcyn="$(tput setab 6)"     # Cyan
bgwht="$(tput setab 7)"     # White
bbgblk="$(tput setab 8)"    # Black - Background - Intense
bbgred="$(tput setab 9)"    # Red
bbggrn="$(tput setab 10)"   # Green
bbgylw="$(tput setab 11)"   # Yellow
bbgblu="$(tput setab 12)"   # Blue
bbgpur="$(tput setab 13)"   # Purple
bbgcyn="$(tput setab 14)"   # Cyan
bbgwht="$(tput setab 15)"   # White
normal="$(tput sgr0)"   # text reset
undrln="$(tput smul)"   # underline
noundr="$(tput rmul)"   # remove underline
mkbold="$(tput bold)"   # make bold
mkblnk="$(tput blink)"  # make blink
revers="$(tput rev)"    # reverse
# }}}

source ~/.bash/prompt

export EDITOR=vim
export PATH=/store/bin/:$HOME/.cabal/bin/:/usr/lib/colorgcc/bin/:$PATH
export CCACHE_PATH="/usr/bin"
alias repomgr='bash /store/dev/repos/bzr/repomgr.sh'
