echo "Installing brew list"

while read p; do
  brew install $p
done < brew_list

echo "Installing oh-my-zsh"

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "Installing brew cask"

while read p; do
  brew cask install $p
done < brew_list_cast

echo "Moving default git and install git with brew"

sudo mv /usr/bin/git /usr/bin/default-git
brew install git

echo "Now create the nvim file"

cat > ~/.config/nvim/init.vim << EOT
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"                                                                              "
"                       __   _ _ _ __ ___  _ __ ___                            "
"                        \ V /| | | | | | | | | (__                            "
"                         \_/ |_|_| |_| |_|_|  \___|                           "
"                                                                              "
"                                                                              "
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

" Starting vim-plug
set mouse-=a
call plug#begin()
	Plug 'Shougo/deoplete.nvim'
	Plug 'zchee/deoplete-go', { 'do': 'make'}
call plug#end()

" Other neo complete settings
" neocomplete like
set completeopt+=noinsert
" deoplete.nvim recommend
set completeopt+=noselect

" Path to python interpreter for neovim
let g:python3_host_prog  = 'python3'

" Run deoplete.nvim automatically
let g:deoplete#enable_at_startup = 1

" Go Related settings
let g:deoplete#sources#go#gocode_binary = '$GOPATH/bin/gocode'
let g:deoplete#sources#go#package_dot = 1
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#pointer = 1
let g:deoplete#sources#go#source_importer = 1
let g:deoplete#sources#go#builtin_objects = 1

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

EOT
