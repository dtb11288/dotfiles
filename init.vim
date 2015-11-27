set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin('~/.config/nvim/bundle')
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
" Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
" Plugin 'user/L9', {'name': 'newL9'}

Plugin 'sidorares/node-vim-debugger'
Plugin 'KabbAmine/vCoolor.vim'
"Plugin 'vim-scripts/VIM-Color-Picker'

" tools
Plugin 'bling/vim-airline'
Plugin 'renyard/vim-git-flow-format'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'kien/ctrlp.vim'
Plugin 'fisadev/vim-ctrlp-cmdpalette'
Plugin 'airblade/vim-gitgutter'
Plugin 'majutsushi/tagbar'
"Plugin 'mbbill/undotree'
"Plugin 'sjl/gundo.vim.git'
Plugin 'simnalamburt/vim-mundo'
Plugin 'qpkorr/vim-bufkill'
"Plugin 'severin-lemaignan/vim-minimap'
Plugin 'vim-scripts/TaskList.vim'

" coding
Plugin 'moll/vim-node'
Plugin 'Valloric/YouCompleteMe'
Plugin 'ternjs/tern_for_vim'
Plugin 'scrooloose/syntastic'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'vim-scripts/simple-pairs'
Plugin 'walm/jshint.vim'
Plugin 'digitaltoad/vim-jade'
Plugin 'Yggdroot/indentLine'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'kana/vim-textobj-user'
Plugin 'vim-scripts/Smart-Tabs'
Plugin 'ntpeters/vim-better-whitespace'

" git
Plugin 'tpope/vim-fugitive'
Plugin 'gregsexton/gitv'

" incsearch
Plugin 'haya14busa/incsearch.vim'
Plugin 'haya14busa/incsearch-fuzzy.vim'

" theme
Plugin 'flazz/vim-colorschemes'
Plugin 'mkarmona/colorsbox'

" javascript
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'

" css
Plugin 'ap/vim-css-color'
Plugin 'hail2u/vim-css3-syntax'

" Snippets manager (SnipMate), dependencies, and snippets repo
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'honza/vim-snippets'
Bundle 'garbas/vim-snipmate'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" autoload at startup
autocmd VimEnter * NERDTreeTabsOpen
"autocmd VimEnter * Minimap
autocmd VimEnter * wincmd p

" maping keys
let mapleader = "\<Space>"
map <F2> :NERDTreeTabsToggle<cr>
map <F8> :TagbarToggle<cr>
"map <F9> :MinimapToggle<cr>
map <leader><F2> :NERDTreeFind<cr>
nnoremap <F5> :GundoToggle<CR>
map <F4> :TaskList<cr>
nnoremap <buffer> k gk
nnoremap <buffer> j gj

" configuration
colorscheme colorsbox-material
set ignorecase
set nowrap
set number
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab
set laststatus=2
set background=dark
set list lcs=tab:\¦\

" set terminal color to 256 bit
if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
    set t_Co=256
endif

" airline
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#format = 'Git_flow_branch_format'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" This allows buffers to be hidden if you've modified a buffer.
" This is almost a must if you wish to use buffers in this way.
set hidden

" To open a new empty buffer
" This replaces :tabnew which I used to bind to this mapping
nmap <leader>T :enew<cr>

" Move to the next buffer
nmap <leader>l :bnext<CR>

" Move to the previous buffer
nmap <leader>h :bprevious<CR>

" Close the current buffer and move to the previous one
" This replicates the idea of closing a tab
nmap <leader>bq :bp <BAR> bd #<CR>

" Show all open buffers and their status
nmap <leader>bl :ls<CR>

" tern on vim
let g:tern_show_argument_hints = 'on_hold'
let g:tern_map_keys = 1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" css color
let g:cssColorVimDoNotMessMyUpdatetime = 1

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
set hlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" fuzzy incsearch
map z/ <Plug>(incsearch-fuzzy-/)
map z? <Plug>(incsearch-fuzzy-?)
map zg/ <Plug>(incsearch-fuzzy-stay)

" gundo
let g:gundo_right = 1

" better-whitespaces
autocmd BufWritePre * StripWhitespace
