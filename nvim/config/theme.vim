" show line number
set number
set relativenumber

" always show status bar
set laststatus=2

" turn on syntax
syntax on

" vim color
set t_Co=256

" hightlight current line
set cursorline

" " better scrolling
set ttyfast
set lazyredraw

" hightlight matched pair
set showmatch

" hide mode (normal, insert)
set noshowmode

" theme
set background=dark
colorscheme preto

" airline
let g:airline#extensions#branch#enabled = 1
let g:airline_powerline_fonts = 0
let g:airline#extensions#branch#format = 'Git_flow_branch_format'
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='minimalist'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''

