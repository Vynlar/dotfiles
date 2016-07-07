let mapleader = ","

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'moll/vim-node'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'kchmck/vim-coffee-script'
Plugin 'jeffkreeftmeijer/vim-numbertoggle'
Plugin 'scrooloose/nerdtree'
Plugin 'wincent/command-t'
Plugin 'tpope/vim-fugitive'
Plugin 'powerline/powerline'
Plugin 'will133/vim-dirdiff'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tpope/vim-surround'
Plugin 'mattn/emmet-vim'
Plugin 'yegappan/grep'
Plugin 'scrooloose/syntastic'
Plugin 'StanAngeloff/php.vim'
Plugin 'ervandew/supertab'
Plugin 'Valloric/YouCompleteMe'
Plugin 'sickill/vim-monokai'
"Plugin 'digitaltoad/vim-jade'

" UltiSnips Start
" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<nop>"
let g:ulti_expand_or_jump_res = 0
function ExpandSnippetOrCarriageReturn()
  let snippet = UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return snippet
  else
    return "\<CR>"
  endif
endfunction
inoremap <expr> <CR> pumvisible() ? "<C-R>=ExpandSnippetOrCarriageReturn()<CR>" : "\<CR>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

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
map <leader>gt :YcmCompleter GetType<CR>
map <leader>rn :YcmCompleter RefactorRename 
map <leader>gtd :YcmCompleter GoToDefinition<CR>
map <leader>gtr :YcmCompleter GoToReferences<CR>

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
