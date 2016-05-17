" vim home
let s:vim_home=expand("~/.config/nvim")
let s:vim_plug="jwhitley/vim-plug"

"*****************************************************************************
" vim-plug install packages
"*****************************************************************************
" install vim-plug
let s:bundle_home=s:vim_home."/bundle"
let s:plug_tool_home=s:bundle_home."/vim-plug"
if !isdirectory(s:plug_tool_home."/.git")
	silent exec "!mkdir -p ".s:bundle_home
	silent exec "!git clone https://github.com/".s:vim_plug.".git ".s:plug_tool_home
	let s:bootstrap=1
endif
exec "set rtp+=".s:plug_tool_home
call plug#begin(s:bundle_home)

" let vim-plug manage itself
Plug 'jwhitley/vim-plug'

" control
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'simnalamburt/vim-mundo'
Plug 'moll/vim-bbye'

" auto complete
Plug 'Shougo/deoplete.nvim'
Plug 'ternjs/tern_for_vim', {'do': 'npm install'}
Plug 'carlitux/deoplete-ternjs'
Plug 'neomake/neomake'
Plug 'jiangmiao/auto-pairs'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" editor
Plug 'tpope/vim-commentary'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'junegunn/vim-easy-align'
Plug 'vim-scripts/Smart-Tabs'
Plug 'ntpeters/vim-better-whitespace'
"Plug 'Yggdroot/indentLine'
Plug 'djoshea/vim-autoread'
Plug 'tmhedberg/matchit'
Plug 'hlissner/vim-multiedit'
Plug 'terryma/vim-expand-region'
Plug 'Chiel92/vim-autoformat'
Plug 'michaeljsmith/vim-indent-object'
Plug 'Konfekt/FastFold'

" csv
Plug 'chrisbra/csv.vim'

" hex
Plug 'fidian/hexmode'

" nodejs
Plug 'moll/vim-node'

" jade
Plug 'digitaltoad/vim-pug'

" javascript
Plug 'jelera/vim-javascript-syntax'

" html
Plug 'othree/html5-syntax.vim'
Plug 'othree/html5.vim'

" css
Plug 'ap/vim-css-color'
Plug 'hail2u/vim-css3-syntax'

" markdown
Plug 'gabrielelana/vim-markdown'

" i3
Plug 'PotatoesMaster/i3-vim-syntax'

" haskell
Plug 'Shougo/vimproc', { 'do': 'make' }
Plug 'eagletmt/neco-ghc'
Plug 'eagletmt/ghcmod-vim'
Plug 'dag/vim2hs'
Plug 'lukerandall/haskellmode-vim'

" java
Plug 'artur-shaik/vim-javacomplete2'
Plug 'hsanson/vim-android'

" conky
Plug 'smancill/conky-syntax.vim'

" git
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'
Plug 'renyard/vim-git-flow-format'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'jreybert/vimagit', { 'branch': 'next' }

" rest tool
Plug 'diepm/vim-rest-console'

" search
Plug 'haya14busa/incsearch.vim'
Plug 'dyng/ctrlsf.vim'

" theme
Plug 'flazz/vim-colorschemes'

if exists("s:bootstrap") && s:bootstrap
	unlet s:bootstrap
	autocmd VimEnter * PlugInstall
endif
call plug#end()


"*****************************************************************************
" basic setup
"*****************************************************************************
" encoding
set encoding=utf-8
set fileencoding=utf-8

" map control keys
let mapleader="\<space>"
let maplocalleader="\<space>"

" fix backspace indent
set backspace=indent,eol,start

" enable hidden buffers
set hidden

" no swap, no backup
set nobackup
set noswapfile

" undo settings
exec "set undodir=".s:vim_home."/undofiles"
set undofile

" turn off compatible with vi
set nocompatible

" show line number
set number
set relativenumber

" terminal
set shell=zsh
tnoremap <esc> <c-\><c-n>

" always show status bar
set laststatus=2

" turn on syntax
syntax on

" vim color
set t_Co=256

" hightlight current line
set cursorline

" better scrolling
set ttyfast
set lazyredraw

" hightlight matched pair
set showmatch

" hide mode (normal, insert)
set noshowmode

" interval update time
set updatetime=100

" load ftplugins and indent files
filetype plugin indent on
filetype indent on

" copy to clipboard
vnoremap <leader>y "+y
vnoremap <leader>d "+d
nnoremap <leader>d "+d
nnoremap <leader>y "+y
nnoremap <leader>yy "+yy
nnoremap <leader>dd "+dd

" paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" search
set grepprg="ag --nogroup --nocolor"
set incsearch
set hlsearch
set ignorecase
nnoremap <silent><esc> :noh<cr><esc>

" theme
set background=dark
colorscheme hybrid_material
if &diff
	colorscheme evening
endif

" buffers
" nnoremap <silent><tab> :bnext<cr>
" nnoremap <silent><s-tab> :bprevious<cr>
nnoremap <silent><leader>bq :Bdelete<cr>
nnoremap <silent><leader>bn :enew<cr>
nnoremap <silent><leader>abq :bufdo Bdelete<cr>

" tabs
nnoremap <silent><c-t> :tabnew<cr>

" switching windows
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l
noremap <c-h> <c-w>h

" split windows
noremap \| :vsplit<cr>
noremap _ :split<cr>

" useful mapping keys
nnoremap <buffer> k gk
nnoremap <buffer> j gj
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

" allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" show preview windows at bottom
function! PreviewDown()
	if !&previewwindow
		silent! wincmd P
	endif
	if &previewwindow
		silent! wincmd J
		silent! wincmd p
	endif
endf
au BufWinEnter * call PreviewDown()

" session automatically load and save session on start/exit.
function! MakeSession()
	 if g:sessionfile != ""
		echo "Saving."
		if (filewritable(g:sessiondir) != 2)
			exe "silent !mkdir -p ".g:sessiondir
			redraw!
		endif
		exe "mksession! ".g:sessionfile
	 endif
endfunction
function! LoadSession()
	 if argc() == 0
		let g:sessiondir = s:vim_home."/sessions".getcwd()
		let g:sessionfile = g:sessiondir."/session.vim"
		if (filereadable(g:sessionfile))
			exe "source ".g:sessionfile
		else
			echo "No session loaded."
		endif
	 else
		let g:sessionfile = ""
		let g:sessiondir = ""
	 endif
endfunction
au VimEnter * nested :call LoadSession()
au VimLeave * :call MakeSession()

" don't wrap line
set wrap
set linebreak
set breakindent

" tab options
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

"*****************************************************************************
" plugins configuration
"*****************************************************************************
" NERDTree
noremap <silent><F2> :NERDTreeToggle<cr>
nnoremap <silent><leader><F2> :NERDTreeFind<cr>
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 30
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
autocmd WinEnter * call s:CloseIfOnlyNerdTreeLeft()
" close all open buffers on entering a window if the only buffer that's left is the NERDTree buffer
function! s:CloseIfOnlyNerdTreeLeft()
	if exists("t:NERDTreeBufName")
		if bufwinnr(t:NERDTreeBufName) != -1
			if winnr("$") == 1
				q
			endif
		endif
	endif
endfunction

" tagbar
map <F8> :TagbarToggle<cr>

" airline
let g:airline#extensions#branch#enabled = 1
let g:airline_powerline_fonts = 0
let g:airline#extensions#branch#format = 'Git_flow_branch_format'
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='wombat'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''

" ctrlp
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__
let g:ctrlp_use_caching = 0
noremap <leader>bl :CtrlPBuffer<cr>
noremap <leader>rf :CtrlPMRUFiles<cr>
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrlp_match_window = 'results:100'
let g:ctrlp_buffer_func = { 'enter': 'CtrlPMappings' }
function! CtrlPMappings()
  nnoremap <buffer> <silent> <c-q> :call <sid>DeleteBuffer()<cr>
endfunction
function! s:DeleteBuffer()
	let path = fnamemodify(getline('.')[2:], ':p')
	let bufn = matchstr(path, '\v\d+\ze\*No Name')
	exec "bd" bufn ==# "" ? path : bufn
	exec "norm \<F5>"
endfunction

" tern on vim
let g:tern_show_signature_in_pum = 1
let g:tern_map_keys = 1

" incsearch
nmap / <Plug>(incsearch-forward)
nmap ? <Plug>(incsearch-backward)
vmap / y<Plug>(incsearch-forward)<c-r>"
vmap ? y<Plug>(incsearch-backward)<c-r>"

" :h g:incsearch#auto_nohlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" gundo
nnoremap <silent><F5> :MundoToggle<cr>
let g:mundo_right = 1

" better-whitespaces
autocmd BufWritePre * StripWhitespace

" neomake
let g:neomake_open_list = 2
autocmd BufWritePost * Neomake

" easy align
xmap <leader>ea <Plug>(EasyAlign)
nmap <leader>ea <Plug>(EasyAlign)

" ctrlsf
nmap <leader>/ <Plug>CtrlSFPrompt
vmap <leader>/ <Plug>CtrlSFVwordPath
nmap <silent><F3> :CtrlSFToggle<cr>

" indent
let g:indentLine_enabled = 1
let g:indentLine_faster  = 1
set list lcs=tab:\¦\ " space in the end

" rest
let g:vrc_cookie_jar = '/tmp/vrc_cookie_jar'

" neco-ghc
let g:necoghc_enable_detailed_browse = 1
let g:haskellmode_completion_ghc = 0

" haskell mod
autocmd BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/google-chrome-stable"
"au FileType haskell nnoremap <buffer> tt :call GHC_ShowType(0)<cr>
"au FileType haskell nnoremap <buffer> <silent> tw :GhcModTypeInsert<cr>
"au FileType haskell nnoremap <buffer> <silent> ts :GhcModSplitFunCase<cr>
"au FileType haskell nnoremap <buffer> <silent> tq :GhcModType<cr>
"au FileType haskell nnoremap <buffer> <silent> te :GhcModTypeClear<cr>

" gitgutter
let g:gitgutter_max_signs = 1000

" vim magit
let g:magit_discard_untracked_do_delete=1

" deoplete.
let g:deoplete#enable_at_startup = 1
set completeopt-=preview
