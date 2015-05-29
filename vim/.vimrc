"set nocompatible              " be iMproved, required
"filetype off                  " required

call plug#begin('~/.vim/plugged')

Plug 'kien/rainbow_parentheses.vim'
"Plug 'hsanson/vim-android'
Plug 'scrooloose/nerdcommenter'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
"Plug 'kien/ctrlp.vim'
"Plug 'tacahiroy/ctrlp-funky'
Plug 'Raimondi/delimitMate'
Plug 'Yggdroot/indentLine'
"Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
"Plug 'altercation/vim-colors-solarized'
Plug 'terryma/vim-multiple-cursors'
"Plug 'elzr/vim-json'
"Plug 'lervag/vim-latex'
Plug 'tpope/vim-surround'
Plug 'rust-lang/rust.vim'
Plug 'phildawes/racer'
Plug 'whatyouhide/vim-gotham'
"Plug 'tpope/vim-repeat' | Plug 'svermeulen/vim-easyclip'

call plug#end()


" Use the Solarized Dark theme
set background=dark
colorscheme gotham

" Make Vim more useful
set nocompatible
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamedplus
" Enhance command-line completion
set wildmenu
" Allow cursor keys in insert mode
set esckeys
" Allow backspace in insert mode
set backspace=indent,eol,start
" Optimize for fast terminal connections
set ttyfast
" Add the g flag to search/replace by default
set gdefault
" Use UTF-8 without BOM
set encoding=utf-8 nobomb
" Change mapleader
let mapleader=","
" Don’t add empty newlines at the end of files
set binary
set noeol
" Centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists("&undodir")
	set undodir=~/.vim/undo
endif
set undofile

" Don’t create backups when editing files in certain directories
set backupskip=/tmp/*,/private/tmp/*

" Respect modeline in files
set modeline
set modelines=4
" Enable per-directory .vimrc files and disable unsafe commands in them
set exrc
set secure
" Enable line numbers
set number
" Enable syntax highlighting
syntax on
" Highlight current line
"set cursorline
" Make tabs as wide as two spaces
set tabstop=4
" Show “invisible” characters
set lcs=tab:▸\ ,eol:\ \,nbsp:_
" set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
set list
" Highlight searches
set hlsearch
" Ignore case of searches
set ignorecase
" Highlight dynamically as pattern is typed
set incsearch
" Always show status line
set laststatus=2
" Enable mouse in all modes
set mouse=a
" Disable error bells
set noerrorbells
" Don’t reset cursor to start of line when moving around.
set nostartofline
" Show the cursor position
set ruler
" Don’t show the intro message when starting Vim
set shortmess=atI
" Show the current mode
set showmode
" Show the filename in the window titlebar
set title
" Show the (partial) command as it’s being typed
set showcmd
" Use relative line numbers
"if exists("&relativenumber")
	"set relativenumber
	"au BufReadPost * set relativenumber
"endif
" Start scrolling three lines before the horizontal window border
set scrolloff=3

" Strip trailing whitespace (,ss)
function! StripWhitespace()
	let save_cursor = getpos(".")
	let old_query = getreg('/')
	:%s/\s\+$//e
	call setpos('.', save_cursor)
	call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace()<CR>
" Save a file as root (,w)
noremap <leader>w :w !sudo tee % > /dev/null<CR>

" Automatic commands
if has("autocmd")
	" Enable file type detection
	filetype plugin indent on
	" Treat .json files as .js
	autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
endif

" Enable pathogen
"execute pathogen#infect()

" swap ; and :
 nnoremap ; :
 nnoremap : ;
 vnoremap ; :
 vnoremap : ;

":au FocusLost * :set number
":au FocusGained * :set relativenumber
autocmd InsertEnter * :set number
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
"autocmd InsertLeave * :set nonumber

" Rainbow Parantheses
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
noremap <leader>m :RainbowParenthesesToggle<CR>


" ==== <UltiSnips> ====
" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" ==== </UltiSnips> ====

" Spaces FTW
set shiftwidth=4
set expandtab

let g:ctrlp_map = '<space>'
"let g:ctrlp_map = '<leader>p'
set wildignore+=*/build/**
let g:android_sdk_path= '/Applications/Android\ Studio.app/sdk/'
let g:android_adb_tool= '/Applications/Android\ Studio.app/sdk/platform-tools/adb'
let gradle_path= '~/.gradle/'
let g:android_build_type= 'gradle'

let delimitMate_expand_space=1
let delimitMate_expand_cr=2

let g:indentLine_enabled=1
"let g:indentLine_color_term = 239
"let g:indentLine_color_gui = '#09AA08'
"let g:indentLine_char = '│'
let g:indentLine_char = '¦'

 " ctrlp {
    if isdirectory(expand("~/.vim/bundle/ctrlp.vim/"))
        let g:ctrlp_working_path_mode = 'ra'
        nnoremap <silent> <D-t> :CtrlP<CR>
        nnoremap <silent> <D-r> :CtrlPMRU<CR>
        let g:ctrlp_custom_ignore = {
            \ 'dir': '\.git$\|\.hg$\|\.svn$',
            \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

" On Windows use "dir" as fallback command.
        if executable('ag')
            let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
        elseif executable('ack-grep')
            let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
        elseif executable('ack')
            let s:ctrlp_fallback = 'ack %s --nocolor -f'
        else
            let s:ctrlp_fallback = 'find %s -type f'
        endif
        let g:ctrlp_user_command = {
            \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
            \ },
            \ 'fallback': s:ctrlp_fallback
        \ }

        if isdirectory(expand("~/.vim/bundle/ctrlp-funky/"))
" CtrlP extensions
            let g:ctrlp_extensions = ['funky']

"funky
            nnoremap <Leader>fu :CtrlPFunky<Cr>
        endif
    endif
"}
"

" change cursor shape based on mode
if &term =~ "xterm\\|rxvt"
  " use a solid vertical bar in insert mode
  let &t_SI .= "\<Esc>[6 q"
  " use a solid block otherwise
  let &t_EI .= "\<Esc>[2 q"
endif
" eliminite the delay when switching modes
set ttimeoutlen=0

set pastetoggle=<leader>p

"set conceallevel=0
"let g:vim_json_syntax_conceal = 0
"let g:indentLine_noConcealCursor=""
" latex mode settings
let g:Tex_DefaultTargetFormat = "pdf"

set hidden
let g:racer_cmd = "racer"
let $RUST_SRC_PATH=expand('~/misc/rs/rust/src/')

noremap \ :q<CR>

