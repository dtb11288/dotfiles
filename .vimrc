set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
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

Plugin 'bling/vim-airline'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'sidorares/node-vim-debugger'
Plugin 'digitaltoad/vim-jade'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'fisadev/vim-ctrlp-cmdpalette'
Plugin 'majutsushi/tagbar'
Plugin 'airblade/vim-gitgutter'
Plugin 'flazz/vim-colorschemes'
Plugin 'walm/jshint.vim'
Plugin 'godlygeek/tabular'
Plugin 'fholgado/minibufexpl.vim' 
Plugin 'qpkorr/vim-bufkill'
Plugin 'ternjs/tern_for_vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/syntastic'
Plugin 'vim-scripts/simple-pairs'
Plugin 'renyard/vim-git-flow-format'
Plugin 'gregsexton/gitv'
Plugin 'mbbill/undotree'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'nathanaelkane/vim-indent-guides' 
Plugin 'KabbAmine/vCoolor.vim'
"Plugin 'vim-scripts/VIM-Color-Picker'

" incsearch
Plugin 'haya14busa/incsearch.vim'
Plugin 'haya14busa/incsearch-fuzzy.vim'

" theme
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
"autocmd VimEnter * NERDTreeTabsOpen
autocmd VimEnter * wincmd p

" maping keys
map <F2> :NERDTreeTabsToggle<cr>
map <F8> :TagbarToggle<cr>
map <F3> :NERDTreeFind<cr>
nmap <F5> :UndotreeToggle<cr>

" configuration
"colorscheme desert256
colorscheme colorsbox-material
set tabstop=4 
set softtabstop=4 
set shiftwidth=4 
set expandtab
set laststatus=2
set background=dark

"au FileType javascript call JavaScriptFold()

""""""""""""""""""""""""""""""
" airline
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#format = 'Git_flow_branch_format'

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

" indent guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 4
let g:indent_guides_enable_on_vim_startup = 1

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
