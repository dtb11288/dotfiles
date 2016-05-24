" vim home
let s:vim_home=expand("~/.config/nvim")

"*****************************************************************************
" dein install packages
"*****************************************************************************
" install dein
let s:bundle_home=s:vim_home."/bundle"
let s:dein_repo="Shougo/dein.vim"
let s:plug_tool_home=s:bundle_home."/repos/github.com/".s:dein_repo
if !isdirectory(s:plug_tool_home."/.git")
	silent exec "!mkdir -p ".s:bundle_home
	silent exec "!git clone https://github.com/".s:dein_repo.".git ".s:plug_tool_home
endif
exec "set rtp+=".s:plug_tool_home
call dein#begin(s:bundle_home)

" let dein manage itself
exec "call dein#add('".s:dein_repo."')"

" control
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('scrooloose/nerdtree')
call dein#add('ctrlpvim/ctrlp.vim')
call dein#add('majutsushi/tagbar')
call dein#add('simnalamburt/vim-mundo')
call dein#add('moll/vim-bbye')
call dein#add('easymotion/vim-easymotion')

" auto complete
call dein#add('Shougo/deoplete.nvim')
call dein#add('ternjs/tern_for_vim', {'build': 'npm install'})
call dein#add('carlitux/deoplete-ternjs')
call dein#add('neomake/neomake')
call dein#add('jiangmiao/auto-pairs')
call dein#add('SirVer/ultisnips')
call dein#add('honza/vim-snippets')

" editor
call dein#add('tomtom/tcomment_vim')
call dein#add('vim-scripts/ReplaceWithRegister')
call dein#add('terryma/vim-multiple-cursors')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-repeat')
call dein#add('junegunn/vim-easy-align')
call dein#add('vim-scripts/Smart-Tabs')
call dein#add('ntpeters/vim-better-whitespace')
"call dein#add('Yggdroot/indentLine')
call dein#add('djoshea/vim-autoread')
call dein#add('tmhedberg/matchit')
call dein#add('hlissner/vim-multiedit')
call dein#add('terryma/vim-expand-region')
call dein#add('Chiel92/vim-autoformat')
call dein#add('Konfekt/FastFold')

" text object
call dein#add('kana/vim-textobj-user')
call dein#add('kana/vim-textobj-indent')
call dein#add('glts/vim-textobj-comment')

" csv
call dein#add('chrisbra/csv.vim')

" hex
call dein#add('fidian/hexmode')

" nodejs
call dein#add('moll/vim-node')

" jade
call dein#add('digitaltoad/vim-pug')

" javascript
call dein#add('jelera/vim-javascript-syntax')

" html
call dein#add('othree/html5-syntax.vim')
call dein#add('othree/html5.vim')

" css
call dein#add('ap/vim-css-color')
call dein#add('hail2u/vim-css3-syntax')

" markdown
call dein#add('gabrielelana/vim-markdown')

" i3
call dein#add('PotatoesMaster/i3-vim-syntax')

" haskell
call dein#add('Shougo/vimproc', { 'build': 'make' })
call dein#add('eagletmt/neco-ghc')
call dein#add('eagletmt/ghcmod-vim')
call dein#add('dag/vim2hs')
call dein#add('lukerandall/haskellmode-vim')

" java
call dein#add('artur-shaik/vim-javacomplete2')
call dein#add('hsanson/vim-android')

" conky
call dein#add('smancill/conky-syntax.vim')

" git
call dein#add('tpope/vim-fugitive')
call dein#add('gregsexton/gitv')
call dein#add('renyard/vim-git-flow-format')
call dein#add('Xuyuanp/nerdtree-git-plugin')
call dein#add('airblade/vim-gitgutter')
call dein#add('jreybert/vimagit', { 'rev': 'next' })

" rest tool
call dein#add('diepm/vim-rest-console')

" search
call dein#add('haya14busa/incsearch.vim')
call dein#add('dyng/ctrlsf.vim')

" theme
call dein#add('flazz/vim-colorschemes')

" end and check install
call dein#end()
if dein#check_install()
	call dein#install()
endif


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

" title
set title
autocmd BufEnter * let &titlestring = expand("%:t")

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

" toggle vexplore
fun! VexToggle(dir, file)
	if exists("t:vex_buf_nr")
		call VexClose()
	else
		call VexOpen(a:dir, a:file)
	endif
endf
fun! VexOpen(dir, file)
	let vex_width = 30
	execute "Vexplore " . a:dir
	let t:vex_buf_nr = bufnr("%")
	if get(b:, 'netrw_liststyle') == 2
		let pattern = '\%(^\|\s\+\)\zs'.escape(a:file, '.*[]~\').'[/*|@=]\=\%($\|\s\+\)'
	else
		let pattern = '^\%(| \)*'.escape(a:file, '.*[]~\').'[/*|@=]\=\%($\|\t\)'
	endif
	call search(pattern, 'wc')
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
map <silent><F9> :call VexToggle(getcwd(), "")<CR>
map <silent><leader><F9> :call VexToggle(expand("%:p:h"), expand("%:t"))<CR>
let g:netrw_browse_split = 4
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_altv = 1
let g:netrw_preview = 1
autocmd FileType netrw setl bufhidden=wipe

" don't wrap line
set wrap
set linebreak
set breakindent

" tab options
set tabstop=2
set softtabstop=2
set shiftwidth=2
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
let g:ctrlp_open_multiple_files = 'i'
let g:ctrlp_match_window = 'results:100'
let g:ctrlp_buffer_func = { 'enter': 'CtrlPBufferMappings' }
function! CtrlPBufferMappings()
	nnoremap <buffer> <silent> <c-q> :call <sid>CtrlPDeleteBuffer()<cr>
endfunction
function! s:CtrlPCloseBuffer(bufline)
	let bufnum = matchlist(a:bufline, '>\s\+\([0-9]\+\)')[1]
	exec "silent! bdelete" bufnum
	return bufnum
endfunction
function! s:CtrlPDeleteBuffer()
	let marked = ctrlp#getmarkedlist()
	if empty(marked)
		let linenum = line('.')
		call s:CtrlPCloseBuffer(getline('.'))
		exec "norm \<F5>"
		let linebottom = line('$')
		if linenum < linebottom
			exec linenum
		endif
	else
		for fname in marked
			let bufid = fname =~ '\[\d\+\*No Name\]$' ? str2nr(matchstr(fname, '\d\+')) : fnamemodify(fname[2:], ':p')
			exec "silent! bdelete" bufid
		endfor
		exec "norm \<F5>"
		call ctrlp#clearmarkedlist()
	endif
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

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
let g:deoplete#file#enable_buffer_path = 1
let g:deoplete#enable_smart_case = 1
set completeopt-=preview
