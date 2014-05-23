set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'Lokaltog/vim-easymotion'
Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bling/vim-airline'
Plugin 'flazz/vim-colorschemes'
Plugin 'kien/ctrlp.vim'
Plugin 'klen/python-mode'
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tmhedberg/matchit'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'pangloss/vim-javascript'

call vundle#end()

filetype plugin indent on
syntax on

set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set showmatch
set title
set cursorline
set list
set listchars=nbsp:¬,tab:»·,trail:·
set background=dark


"
" Style
"
set t_Co=256
"colorscheme molokai

colorscheme mustang
hi CursorLine cterm=NONE

"colorscheme xoria256
:let g:zenburn_high_Contrast = 1
"colorscheme zenburn
"hi Normal ctermbg=NONE
"hi NonText ctermbg=NONE

"highlight ColorColumn ctermbg=233 guibg=#2c2d27
"set colorcolumn=80
highlight OverLength ctermbg=52 ctermfg=white
match OverLength /\%>79v.\+/


" Python
autocmd FileType python setlocal nosmartindent
"autocmd BufWritePost *.py call Flake8()

"
" pymode
"
"let g:pymode = 1
let g:pymode_trim_whitespaces = 1
let g:pymode_options = 1
let g:pymode_folding = 0
let g:pymode_motion = 1
let g:pymode_virtualenv = 0
"let g:pymode_rope = 1
let g:pymode_rope_completion = 0
"let g:pymode_lint = 1
let g:pymode_lint_on_write = 1
let g:pymode_lint_on_fly = 0
"let g:pymode_lint_checkers = ['pep8', 'mccabe']

autocmd BufWritePre *.go Fmt

" Django
autocmd BufRead,BufNewFile *.html set filetype=htmldjango

" Delete unwanted whitespaces on save
autocmd BufWritePre * :%s/\s\+$//e
autocmd BufWritePre * :%s/^\s\+$//e

"
" Leader
"
let mapleader=","
nnoremap <Leader>w :w<CR>
nnoremap <Leader>e :Explore<CR>
vnoremap v <ESC>

nnoremap <Leader>f :CtrlP<CR>

"
" CtrlP
"
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]((\.(git|hg|svn))|venv[a-z0-9-]*|__pycache__)$',
  \ 'file': '\v\.(exe|so|dll|pyc|o)$'
  \ }

"Use TAB to complete when typing words, else inserts TABs as usual.
"Uses dictionary and source files to find matching words to complete.
"See help completion for source,
"Note: usual completion is on <C-n> but more trouble to press all the time.
"Never type the same word twice and maybe learn a new spellings!
"Use the Linux dictionary when spelling is in doubt.
"Window users can copy the file to their machine.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>

"
" EasyMotion
"
nmap <Leader><Leader>s <Plug>(easymotion-s2)
nmap <Leader><Leader>t <Plug>(easymotion-t2)

map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

let g:EasyMotion_smartcase = 1


"
" Line numbers
"
set number
set relativenumber
function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

"
" Airline
"
set laststatus=2
set ttimeoutlen=50
let g:bufferline_echo = 0
let g:airline_powerline_fonts = 0
let g:airline_theme = 'zenburn'

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = 'ln:'
let g:airline_symbols.branch = 'br:'
let g:airline_symbols.paste = 'paste'
let g:airline_symbols.whitespace = 'Ξ'


"
" Websearch
"
command! -nargs=1 Browser
            \ | execute ':silent !xdg-open "'.<q-args>.'"'
            \ | execute ':redraw!'

nnoremap <Leader>sp :Browser http://php.net/
nnoremap <Leader>sg :Browser http://google.com/?q=
