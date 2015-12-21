"" Bundle home
let s:vim_home=expand("~/.vim")

"*****************************************************************************
"" Vim-plug install packages
"*****************************************************************************
"" Install Vim-plug
let s:bundle_home=s:vim_home."/bundle"
let s:plug_tool_home=s:bundle_home."/vim-plug"

if !isdirectory(s:plug_tool_home."/.git")
    silent exec "!mkdir -p ".s:bundle_home
    silent exec "!git clone https://github.com/jwhitley/vim-plug.git ".s:plug_tool_home
    let s:bootstrap=1
endif

exec "set rtp+=".s:plug_tool_home
call plug#begin(s:bundle_home)

"" Let Vim-plug manage Vim-plug
Plug 'jwhitley/vim-plug'

"" Control
Plug 'L9'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdtree'
"Plug 'jistr/vim-nerdtree-tabs'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
"Plug 'mbbill/undotree'
Plug 'simnalamburt/vim-mundo'
Plug 'qpkorr/vim-bufkill'
Plug 'vim-scripts/confirm-quit'
Plug 'tpope/vim-vinegar'

"" Auto complete
Plug 'Valloric/YouCompleteMe', {'do': './install.py'}
Plug 'ternjs/tern_for_vim', {'do': 'npm install'}
Plug 'scrooloose/syntastic'
Plug 'jiangmiao/auto-pairs'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

"" Editor
Plug 'scrooloose/nerdcommenter'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'kana/vim-textobj-user'
Plug 'vim-scripts/Smart-Tabs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Yggdroot/indentLine'
Plug 'djoshea/vim-autoread'

"" Hex
Plug 'fidian/hexmode'

"" Nodejs
Plug 'moll/vim-node'
Plug 'sidorares/node-vim-debugger'

"" Jade
Plug 'digitaltoad/vim-jade'

"" Javascript
Plug 'walm/jshint.vim'
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'

"" CSS
Plug 'ap/vim-css-color'
Plug 'hail2u/vim-css3-syntax'

"" Git
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'
Plug 'renyard/vim-git-flow-format'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'jreybert/vimagit', { 'branch': 'next' }

"" Search
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'dyng/ctrlsf.vim'
Plug 'rking/ag.vim'
Plug 'gabesoft/vim-ags'

"" Theme
Plug 'flazz/vim-colorschemes'
Plug 'mkarmona/colorsbox'

if exists("s:bootstrap") && s:bootstrap
    unlet s:bootstrap
    autocmd VimEnter * PlugInstall
endif

call plug#end()


"*****************************************************************************
"" Basic setup
"*****************************************************************************"
"" Encoding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

"" Fix backspace indent
set backspace=indent,eol,start

"" Enable hidden buffers
set hidden

"" Directories for swp files
set nobackup
set noswapfile

"" Map control keys
let mapleader=','
nnoremap ; :

"" Config
set nocompatible
set number
set shell=zsh
set laststatus=2
syntax on
set t_Co=256
set cursorline
set ttyfast " u got a fast terminal
set lazyredraw " to avoid scrolling problems
set showmatch

"" Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

"" Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

"" Terminal
tnoremap <Esc> <C-\><C-n>

"" Search
set grepprg=ag\ --nogroup\ --nocolor
set hlsearch
set ignorecase

"" Theme
colorscheme colorsbox-material

"" Tabs
nnoremap <silent> <tab> :bnext<CR>
nnoremap <silent> <s-tab> :bprevious<CR>
"nnoremap <silent> <leader>bq :bp <BAR> bd! #<CR>
nnoremap <silent> <leader>bq :bp<bar>sp<bar>bn<bar>bd!<CR>
nnoremap <silent> <leader>bn :enew<CR>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Switching windows
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

"" Mapping keys
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

"" Session
" automatically load and save session on start/exit.
" Modified by robin burchell <w00t@inspircd.org> to only load/save sessions if started with no arguments.
function! MakeSession()
	if g:sessionfile != ""
		echo "Saving."
		if (filewritable(g:sessiondir) != 2)
			exe 'silent !mkdir -p ' g:sessiondir
			redraw!
		endif
		exe "mksession! " . g:sessionfile
	endif
endfunction

function! LoadSession()
	if argc() == 0
		let g:sessiondir = s:vim_home."/sessions" . getcwd()
		let g:sessionfile = g:sessiondir . "/session.vim"
		if (filereadable(g:sessionfile))
			exe 'source ' g:sessionfile
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

" Toggle Vexplore with Ctrl-E
fun! VexToggle()
  if exists("t:vex_buf_nr")
    call VexClose()
  else
    call VexOpen()
  endif
endf
fun! VexOpen()
  let g:netrw_browse_split=4    " open files in previous window
  let vex_width = 25

  execute "Vexplore " . getcwd()
  let t:vex_buf_nr = bufnr("%")
  wincmd H

  call VexSize(vex_width)
endf
fun! VexClose()
  let cur_win_nr = winnr()
  let target_nr = ( cur_win_nr == 1 ? winnr("#") : cur_win_nr )

  1wincmd w
  close
  unlet t:vex_buf_nr

  execute (target_nr - 1) . "wincmd w"
  call NormalizeWidths()
endf
fun! VexSize(vex_width)
  execute "vertical resize" . a:vex_width
  set winfixwidth
  call NormalizeWidths()
endf
fun! NormalizeWidths()
  let eadir_pref = &eadirection
  set eadirection=hor
  set equalalways! equalalways!
  let &eadirection = eadir_pref
endf
map <silent> <F3> :call VexToggle()<CR>
let g:netrw_liststyle=3         " thin (change to 3 for tree)
let g:netrw_banner=0            " no banner
let g:netrw_altv=1              " open files on right
let g:netrw_preview=1           " open previews vertically

"" Whitespace
set nowrap
set tabstop=4
set softtabstop=0
set shiftwidth=4
set noexpandtab
set list

"*****************************************************************************
"" Plugins configuration
"*****************************************************************************
"" NERDTree
"autocmd VimEnter * NERDTreeTabsOpen
"autocmd VimEnter * wincmd p
nnoremap <silent> <leader><F2> :NERDTreeFind<CR>
noremap <F2> :NERDTreeToggle<CR>
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 30
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
autocmd WinEnter * call s:CloseIfOnlyNerdTreeLeft()
" Close all open buffers on entering a window if the only
" buffer that's left is the NERDTree buffer
function! s:CloseIfOnlyNerdTreeLeft()
	if exists("t:NERDTreeBufName")
		if bufwinnr(t:NERDTreeBufName) != -1
			if winnr("$") == 1
				q
			endif
		endif
	endif
endfunction

"" Tagbar
map <F8> :TagbarToggle<CR>

"" Airline
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#format = 'Git_flow_branch_format'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

"" Ctrlp
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__
let g:ctrlp_use_caching = 0
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
noremap <leader>bl :CtrlPBuffer<CR>
noremap <leader>e :CtrlPMRUFiles<CR>
"let g:ctrlp_map = '<leader>e'
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_dont_split = 'NERD_tree_2'

"" Tern on vim
let g:tern_show_argument_hints = 'on_hold'
let g:tern_map_keys = 1
nmap <leader>td :TernDef<cr>
nmap <leader>tb :TernDocBrowse<cr>
nmap <leader>tt :TernType<cr>
nmap <leader>to :TernDoc<cr>
nmap <leader>tpd :TernDefPreview<cr>
nmap <leader>tsd :TernDefSplit<cr>
nmap <leader>ttd :TernDefTab<cr>
nmap <leader>tr :TernRefs<cr>
nmap <leader>tR :TernRename<cr>

"" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1
let g:syntastic_aggregate_errors = 1
"let g:syntastic_javascript_checkers = ['eslint']
"let g:syntastic_javascript_eslint_exec = 'eslint_d'

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" fuzzy incsearch
map z/  <Plug>(incsearch-easymotion-/)
map z?  <Plug>(incsearch-easymotion-?)
map zg/ <Plug>(incsearch-easymotion-stay)

" incsearch.vim x fuzzy x vim-easymotion

function! s:config_easyfuzzymotion(...) abort
	return extend(copy({
	\	 'converters': [incsearch#config#fuzzy#converter()],
	\	 'modules': [incsearch#config#easymotion#module()],
	\	 'keymap': {"\<CR>": '<Over>(easymotion)'},
	\	 'is_expr': 0,
	\	 'is_stay': 1
	\ }), get(a:, 1, {}))
endfunction

noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())

"" Gundo
nnoremap <F5> :GundoToggle<CR>
let g:gundo_right = 1

"" Better-whitespaces
autocmd BufWritePre * StripWhitespace

"" Git
noremap <leader>ga  :Gwrite<CR>
noremap <leader>gR  :Gread<CR>
noremap <leader>gc  :Gcommit<CR>
noremap <leader>gca :Gcommit --amend<CR>
noremap <leader>gsh :Gpush<CR>
noremap <leader>gll :Gpull<CR>
noremap <leader>gs  :Gstatus<CR>
noremap <leader>gb  :Gblame<CR>
noremap <leader>gd  :Gvdiff<CR>
noremap <leader>gr  :Gremove<CR>

"" Easy align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"" CtrlSF
nmap f/ <Plug>CtrlSFPrompt
vmap f/ <Plug>CtrlSFVwordPath

"" Indent
set list lcs=tab:\¦\ "need space at the end
