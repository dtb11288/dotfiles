" session automatically load and save session on start/exit.
function! MakeSession()
  if g:sessionfile != ""
    echo "Saving."
    if (filewritable(g:sessiondir) != 2)
      exec "silent !mkdir -p ".g:sessiondir
      redraw!
    endif
    exe "mksession! ".g:sessionfile
  endif
endfunction
function! LoadSession()
  if argc() == 0
    let g:sessiondir = g:vim_home."/sessions".getcwd()
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

