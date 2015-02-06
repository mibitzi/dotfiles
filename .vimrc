set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'Lokaltog/vim-easymotion'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bling/vim-airline'
Plugin 'flazz/vim-colorschemes'
Plugin 'tomasr/molokai'
Plugin 'kien/ctrlp.vim'
Plugin 'klen/python-mode'
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tmhedberg/matchit'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'pangloss/vim-javascript'
Plugin 'sjl/gundo.vim'
Plugin 'scrooloose/syntastic'
Plugin 'evidens/vim-twig'
Plugin 'wting/rust.vim'
Plugin 'fatih/vim-go'
Plugin 'jiangmiao/auto-pairs'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'derekwyatt/vim-scala'
Plugin 'scrooloose/nerdtree'

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
set incsearch

" Show whitespaces
set list
set listchars=nbsp:¬,tab:»·,trail:·


"
" Style
"
set background=dark
set t_Co=256

"let g:rehash256 = 1
"colorscheme molokai
"hi MatchParen ctermfg=208  ctermbg=233 cterm=bold

colorscheme mustang
"colorscheme xoria256
hi CursorLine cterm=NONE

"let g:zenburn_high_Contrast = 1
"hi Normal ctermbg=NONE
"hi NonText ctermbg=NONE
"colorscheme zenburn


"highlight ColorColumn ctermbg=233 guibg=#2c2d27
"set colorcolumn=80
"highlight OverLength ctermbg=52 ctermfg=white
"match OverLength /\%>79v.\+/


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
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
"let g:pymode_lint = 1
let g:pymode_lint_on_write = 1
let g:pymode_lint_on_fly = 0
"let g:pymode_lint_checkers = ['pep8', 'mccabe']

" Go
autocmd FileType go set noexpandtab nolist
"autocmd BufWritePre *.go silent Fmt
let g:go_fmt_fail_silently = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1

" Django
autocmd BufRead,BufNewFile *.html set filetype=htmldjango

" Markdown
let g:vim_markdown_folding_disabled=1

" Rust fix
au BufRead,BufNewFile *.rs set filetype=rust

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

"
" CtrlP
"
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]((\.(git|hg|svn))|venv[a-z0-9-]*|__pycache__|node_modules|reports\/test)$',
  \ 'file': '\v\.(exe|so|dll|pyc|o|class|jar)$'
  \ }

nnorema <Leader>f :CtrlP<CR>
nnorema <Leader>b :CtrlPBuffer<CR>


"
" EasyMotion
"
"nmap <Leader>m <Plug>(easymotion-s2)

"map  / <Plug>(easymotion-sn)
"omap / <Plug>(easymotion-tn)

nmap <SPACE> <Plug>(easymotion-s2)
"nmap t <Plug>(easymotion-t2)

"map <Leader>l <Plug>(easymotion-lineforward)
"map <Leader>j <Plug>(easymotion-j)
"map <Leader>k <Plug>(easymotion-k)
"map <Leader>h <Plug>(easymotion-linebackward)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion
let g:EasyMotion_smartcase = 1


"
" Line numbers
"
set number
"set relativenumber
"function! NumberToggle()
"  if(&relativenumber == 1)
"    set norelativenumber
"  else
"    set relativenumber
"  endif
"endfunc

nnoremap <C-l> :call NumberToggle()<cr>

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
" UltiSnip
"
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"


"
" Gundo
"
nnoremap <Leader>g :GundoToggle<Cr>


"
" YouCompleteMe
"
set completeopt=menu,menuone
let g:EclimCompletionMethod = 'omnifunc'


"
" NERDTree
"
map <Leader>t :NERDTreeToggle<CR>
