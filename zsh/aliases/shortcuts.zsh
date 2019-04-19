# useful shortcuts
alias cclip='xclip -selection clipboard'
alias showuser='cut -d: -f1 /etc/passwd'
alias my-ip="curl http://ipecho.net/plain; echo"
alias fetch="neofetch"

alias viml="vim -u $HOME/dotfiles/vim/vimrc.noplugin"
alias vimt="vim -u $HOME/dotfiles/vim/vimrc.testing"

alias rtorrent-attach="tmux -L rtorrent -S /tmp/rtorrent attach -t rtorrent"

# directories
alias cmu='cd ~/Documents/CMU'
alias books='cd ~/Documents/Books\ and\ Papers'

# files
alias zshrc='source ~/.zshrc'
alias edit-vim='$EDITOR ~/.vimrc'
alias edit-latex='$EDITOR ~/texmf/tex/latex/local/anishs.sty'
alias edit-zsh='$EDITOR ~/.zshrc'
