" vim home
let g:vim_home=expand("~/.config/nvim")
let s:bundle_home=g:vim_home."/bundle"
let s:vim_plug_repo="junegunn/vim-plug"
let s:plug_tool_home=s:bundle_home."/vim-plug"
if !isdirectory(s:plug_tool_home."/.git")
  silent exec "!mkdir -p ".s:bundle_home
  silent exec "!git clone https://github.com/".s:vim_plug_repo.".git ".s:plug_tool_home
  silent exec "!mkdir -p ".s:plug_tool_home."/autoload"
  silent exec "!ln -s ".s:plug_tool_home."/plug.vim ".s:plug_tool_home."/autoload"
  let s:bootstrap=1
endif
exec "set rtp+=".s:plug_tool_home
call plug#begin(s:bundle_home)

" let vim_plug manage itself
exec "Plug '".s:vim_plug_repo."'"

" control
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'simnalamburt/vim-mundo'
Plug 'haya14busa/incsearch.vim'
Plug 'dyng/ctrlsf.vim'

" editor
Plug 'tomtom/tcomment_vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'junegunn/vim-easy-align'
Plug 'ntpeters/vim-better-whitespace'
Plug 'djoshea/vim-autoread'
Plug 'Chiel92/vim-autoformat'
Plug 'moll/vim-bbye'
Plug 'jiangmiao/auto-pairs'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'neomake/neomake'

" core complete
" if has('nvim')
"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif
" Plug 'autozimu/LanguageClient-neovim', {'branch': 'next', 'do': 'bash install.sh'}
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" rest tool
Plug 'diepm/vim-rest-console'

" theme
Plug 'ewilazarus/preto'

" git
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'
Plug 'renyard/vim-git-flow-format'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'jreybert/vimagit'

" rust
Plug 'rust-lang/rust.vim'

" javscript
Plug 'pangloss/vim-javascript'

" python
Plug 'vim-python/python-syntax'

" json
Plug 'elzr/vim-json'

" elm
Plug 'ElmCast/elm-vim'

" dart
Plug 'dart-lang/dart-vim-plugin'

" toml
Plug 'cespare/vim-toml'

" haskell
Plug 'neovimhaskell/haskell-vim'

" typescript
Plug 'leafgarland/typescript-vim'

" html
Plug 'othree/html5-syntax.vim'
Plug 'othree/html5.vim'
Plug 'alvan/vim-closetag'

" css
Plug 'ap/vim-css-color'
Plug 'hail2u/vim-css3-syntax'

" markdown
Plug 'gabrielelana/vim-markdown'

" nix
Plug 'LnL7/vim-nix'

" solidity
Plug 'tomlion/vim-solidity'

" end and check install
call plug#end()
if exists("s:bootstrap") && s:bootstrap
  unlet s:bootstrap
  exec "PlugInstall"
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
exec "set undodir=".g:vim_home."/undofiles"
if (filewritable(g:vim_home."/undofiles") != 2)
  exec "silent !mkdir -p ".g:vim_home."/undofiles"
  redraw!
endif
set undofile

" turn off compatible with vi
set nocompatible

" interval update time
set updatetime=100

" load ftplugins and indent files
filetype plugin indent on
filetype indent on

" buffers
nnoremap <silent><leader>bd :Bdelete<cr>
nnoremap <silent><leader>bn :enew<cr>
nnoremap <silent><leader>abd :bufdo Bdelete<cr>

" switching windows
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l
noremap <c-h> <c-w>h

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

" search
set grepprg="ag --nogroup --nocolor"
set incsearch
set hlsearch
set ignorecase
nnoremap <silent><esc> :noh<cr><esc>

" terminal
tnoremap <esc> <c-\><c-n>

" don't wrap line
set wrap
set linebreak
set breakindent

" tab options
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

" title
set title
set titlestring=%t%(\ -\ [%{substitute(getcwd(),\ $HOME,\ '~',\ '')}]%)

" fix function keys
map <Esc>OP <F1>
map <Esc>OQ <F2>
map <Esc>OR <F3>
map <Esc>OS <F4>
map <Esc>[16~ <F5>
map <Esc>[17~ <F6>
map <Esc>[18~ <F7>
map <Esc>[19~ <F8>
map <Esc>[20~ <F9>
map <Esc>[21~ <F10>
map <Esc>[23~ <F11>
map <Esc>[24~ <F12>

" gvim
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

"*****************************************************************************
" plugins configuration
"*****************************************************************************
" NERDTree
nmap <silent><F2> :NERDTreeToggle<cr>
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

" ctrlp
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__
let g:ctrlp_use_caching = 0
noremap <leader>bl :CtrlPBuffer<cr>
noremap <leader>rf :CtrlPMRUFiles<cr>
noremap <leader>ct :CtrlPTag<cr>
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrlp_open_multiple_files = 'i'
let g:ctrlp_extensions = ['tag']
let g:ctrlp_match_window = 'results:100'

" incsearch
nmap / <Plug>(incsearch-forward)
nmap ? <Plug>(incsearch-backward)
vmap / y<Plug>(incsearch-forward)<c-r>"
vmap ? y<Plug>(incsearch-backward)<c-r>"

" incsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" ctrlsf
nmap <leader>/ <Plug>CtrlSFPrompt
vmap <leader>/ <Plug>CtrlSFVwordPath
nmap <silent><F3> :CtrlSFToggle<cr>

" better-whitespaces
autocmd BufWritePre * StripWhitespace
" neomake
call neomake#configure#automake('w')
let g:neomake_open_list = 4

" ultisnips
let g:UltiSnipsSnippetsDir = g:vim_home."/UltiSnips"

" rest
let g:vrc_curl_opts = {
      \ '--compressed': '',
      \ '--connect-timeout' : 10,
      \ '-b': '/tmp/vrc_cookie_jar',
      \ '-c': '/tmp/vrc_cookie_jar',
      \ '-L': '',
      \ '-i': '',
      \ '--max-time': 60,
      \ '--ipv4': '',
      \ '-k': '',
      \}

" Go to tab by number
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt

" mundo
nnoremap <silent><F5> :MundoToggle<cr>
let g:mundo_right = 1

" autoformat
nnoremap <leader>f :Autoformat<cr>

" gitgutter
let g:gitgutter_max_signs = 1000

" vim magit
let g:magit_discard_untracked_do_delete=1

" android
nnoremap <silent><leader>rr :!adb shell input text "RR"<cr><cr>
nnoremap <silent><leader>am :!adb shell input keyevent 82<cr><cr>

for f in split(glob(g:vim_home.'/config/*.vim'), '\n')
  exe 'source' f
endfor
