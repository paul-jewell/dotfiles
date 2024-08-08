# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]]; then
  # Shell is non-interactive.  Be done now!
  return
fi

# Put your fun stuff here.

export PATH=/home/paul/bin:/home/paul/.local/bin:/home/paul/.cargo/bin:$PATH
HISTCONTROL=ignoreboth
shopt -s histappend
PROMPT_COMMAND+=('history -a; history -n')
HISTIGNORE="ls:ll:cd:pwd:bg:fg:history"

HISTSIZE=100000
HISTFILESIZE=10000000

EDITOR=/usr/bin/nvim
alias bbb="ssh paul@navigator"
alias vim=nvim
alias sbcl="rlwrap sbcl"
alias nextcloud-ssh="ssh -p7264 10.1.1.13"
alias ls=eza
alias ll="eza -l -g --icons"
alias lla="ll -a"
alias cat=bat
export SCIPY_PIL_IMAGE_VIEWER=display

PATH="/home/paul/perl5/bin${PATH:+:${PATH}}"
export PATH
PERL5LIB="/home/paul/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/home/paul/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base \"/home/paul/perl5\""
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=/home/paul/perl5"
export PERL_MM_OPT
# Ensure gpg uses the correct key for decrypting passwords, without asking for the yubikey
# This is currently not working - pass still asks for the yubikey...
# export PASSWORD_STORE_GPG_OPTS="--default-key 32089A9C90EAF4BB"

eval "$(starship init bash)"
# Use fish in place of bash
# keep this line at the bottom of ~/.bashrc
#[ -x /bin/fish ] && SHELL=/bin/fish exec fish
