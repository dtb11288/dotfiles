" vim home
let s:vim_home=expand("~/.config/nvim")

"*****************************************************************************
" vim-plug install packages
"*****************************************************************************
" install vim-plug
let s:bundle_home=s:vim_home."/bundle"
let s:plug_tool_home=s:bundle_home."/vim-plug"
if !isdirectory(s:plug_tool_home."/.git")
	silent exec "!mkdir -p ".s:bundle_home
	silent exec "!git clone https://github.com/jwhitley/vim-plug.git ".s:plug_tool_home
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
Plug 'Valloric/YouCompleteMe', {'do': 'python2 install.py'}
Plug 'ternjs/tern_for_vim', {'do': 'npm install'}
Plug 'scrooloose/syntastic'
Plug 'jiangmiao/auto-pairs'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" editor
Plug 'scrooloose/nerdcommenter'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'junegunn/vim-easy-align'
Plug 'vim-scripts/Smart-Tabs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Yggdroot/indentLine'
Plug 'djoshea/vim-autoread'
Plug 'tmhedberg/matchit'
Plug 'hlissner/vim-multiedit'

" csv
Plug 'chrisbra/csv.vim'

" hex
Plug 'fidian/hexmode'

" nodejs
Plug 'moll/vim-node'
Plug 'sidorares/node-vim-debugger'

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
Plug 'easymotion/vim-easymotion'
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
let mapleader=','
let maplocalleader=','
nnoremap ; :

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

" terminal
set shell=zsh
tnoremap <Esc> <C-\><C-n>

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
nnoremap <leader>Y "+yg_
nnoremap <leader>y "+y
nnoremap <leader>yy "+yy

" paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" search
set grepprg="ag --nogroup --nocolor"
set hlsearch
set ignorecase

" theme
set background=dark
colorscheme hybrid_material
if &diff
	colorscheme evening
endif

" buffers
nnoremap <silent><tab> :bnext<CR>
nnoremap <silent><S-tab> :bprevious<CR>
nnoremap <silent><leader>bq :Bdelete<CR>
nnoremap <silent><leader>bn :enew<CR>
nnoremap <silent><leader>abq :bufdo Bdelete<CR>

" tabs
nnoremap <silent><C-t> :tabnew<CR>

" switching windows
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

" split windows
noremap \| :vsplit<CR>
noremap _ :split<CR>

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
noremap <silent><F2> :NERDTreeToggle<CR>
nnoremap <silent><leader><F2> :NERDTreeFind<CR>
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
map <F8> :TagbarToggle<CR>

" airline
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_powerline_fonts = 0
let g:airline#extensions#branch#format = 'Git_flow_branch_format'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='wombat'

" ctrlp
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__
let g:ctrlp_use_caching = 0
cnoremap <C-P> <C-R>=expand("%:p:h")."/" <CR>
noremap <leader>bl :CtrlPBuffer<CR>
noremap <leader>rf :CtrlPMRUFiles<CR>
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_dont_split = 'NERD_tree_2'

" tern on vim
let g:tern_show_signature_in_pum = 1
let g:tern_map_keys = 1

" youcompleteme
let g:ycm_add_preview_to_completeopt = 0
let g:ycm_confirm_extra_conf = 0
set completeopt-=preview

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_jade_checkers = ['jade_lint']

" incsearch
nmap / <Plug>(incsearch-forward)
vmap / y<Plug>(incsearch-forward)<C-R>"

" :h g:incsearch#auto_nohlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" gundo
nnoremap <silent><F5> :MundoToggle<CR>
let g:mundo_right = 1

" better-whitespaces
autocmd BufWritePre * StripWhitespace

" easy align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" ctrlsf
nmap ? <Plug>CtrlSFPrompt
vmap ? <Plug>CtrlSFVwordPath
nmap <silent><F3> :CtrlSFToggle<CR>

" indent
let g:indentLine_enabled = 1
let g:indentLine_faster  = 1
set list lcs=tab:\¦\ " space in the end

" rest
let g:vrc_cookie_jar = '/tmp/vrc_cookie_jar'

" neco-ghc
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:necoghc_enable_detailed_browse = 1
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" haskell mod
autocmd BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/google-chrome-stable"
au FileType haskell nnoremap <buffer> tt :call GHC_ShowType(0)<CR>
au FileType haskell nnoremap <buffer> <silent> tw :GhcModTypeInsert<CR>
au FileType haskell nnoremap <buffer> <silent> ts :GhcModSplitFunCase<CR>
au FileType haskell nnoremap <buffer> <silent> tq :GhcModType<CR>
au FileType haskell nnoremap <buffer> <silent> te :GhcModTypeClear<CR>

" vim magit
let g:magit_discard_untracked_do_delete=1
