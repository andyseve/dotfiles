if [ ! -d ~/texmf/tex/latex/local ]; then
  mkdir -p ~/texmf/tex/latex/local
fi

ln -s ~/dotfiles/latex/anishs.sty ~/texmf/tex/latex/local/anishs.sty

