filetype off

call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on
syntax on

set nocompatible
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set showmatch
set number
set title
set cursorline
"set mouse=a
set background=dark

set t_Co=256
colorscheme xoria256
hi Normal ctermbg=NONE
hi NonText ctermbg=NONE

"highlight ColorColumn ctermbg=233 guibg=#2c2d27
"set colorcolumn=80
highlight OverLength ctermbg=52 ctermfg=white
match OverLength /\%>79v.\+/

autocmd FileType python setlocal nosmartindent
"autocmd BufWritePost *.py call Flake8()

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

let mapleader=","
nnoremap <Leader>w :w<CR>
nnoremap <Leader>e :Explore<CR>
vnoremap v <ESC>

nnoremap <Leader>f :CtrlP<CR>

" CtrlP
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

