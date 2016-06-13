let mapleader = ","

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
"Plugin 'marijnh/tern_for_vim'
Plugin 'moll/vim-node'
Plugin 'digitaltoad/vim-jade'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'kchmck/vim-coffee-script'
Plugin 'Valloric/YouCompleteMe'
Plugin 'jeffkreeftmeijer/vim-numbertoggle'
Plugin 'scrooloose/nerdtree'
Plugin 'wincent/Command-T'
Plugin 'tpope/vim-fugitive'
Plugin 'powerline/powerline'
Plugin 'will133/vim-dirdiff'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tpope/vim-surround'
Plugin 'mattn/emmet-vim'
Plugin 'yegappan/grep'
Plugin 'scrooloose/syntastic'

" UltiSnips Start
" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
"UltiSnips End

call vundle#end()
filetype plugin indent on

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

syntax on 

"inoremap ( ()<Esc>i
"inoremap { {}<Esc>i
"inoremap [ []<Esc>i

imap jk <esc> 
map <C-n> :NERDTreeToggle<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
map <leader>c :!pushd ~/aviata/gambit-incept-web-ui/src && grunt copy && grunt
      \ less && grunt postcss && popd<CR><CR>
map <leader>sa :Rgrep<CR>

autocmd vimenter * NERDTree

set rtp+=$HOME/.local/lib/python2.7/site-packages/powerline/bindings/vim/
set directory=$HOME/.vim/swapfiles//
set number
set tabstop=2
set expandtab
set shiftwidth=2
set autoindent
set smartindent
set laststatus=2
set t_Co=256
