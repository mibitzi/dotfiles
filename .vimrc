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
"Plugin 'fatih/molokai'
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
" Plugin 'scrooloose/nerdtree'
Plugin 'leafgarland/typescript-vim'
Plugin 'facebook/vim-flow'
Plugin 'cakebaker/scss-syntax.vim'

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
"let g:molokai_original = 1
"colorscheme molokai
"hi MatchParen ctermfg=208  ctermbg=233 cterm=bold

colorscheme mustang
hi CursorLine cterm=NONE

"colorscheme xoria256

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
let g:go_fmt_autosave = 1
let g:go_fmt_fail_silently = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 0
let g:go_fmt_command = "goimports"

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

vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

"
" CtrlP
"
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]((\.(git|hg|svn))|venv[a-z0-9-]*|__pycache__|node_modules|reports\/test|app\/cache|vendor)$',
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

"nnoremap <C-l> :call NumberToggle()<cr>


"
" Syntastic
"
let g:syntastic_go_checkers = ['go']


"
" Airline
"
set laststatus=2
set ttimeoutlen=50
let g:bufferline_echo = 0
let g:airline_powerline_fonts = 1
let g:airline_theme = 'badwolf'


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
" Persistent undo
"
" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif


"
" YouCompleteMe
"
set completeopt=menu,menuone
let g:EclimCompletionMethod = 'omnifunc'


"
" Flow
"
let g:flow#autoclose = 1


"
" NERDTree
"
"map <Leader>t :NERDTreeToggle<CR>
"let g:NERDTreeWinSize=20

"" Auto-close if NERDTree is the only window left open
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"
" GVIM
"
if has('gui_running')
    set guifont=Source\ Code\ Pro\ 11
    set guioptions-=T
    set guioptions-=m
    set guioptions-=r
    set guioptions-=R
    set guioptions-=l
    set guioptions-=L
endif
