" scott.vimarc@h4ck3r.net, 2008.
"
" Hacked together based on VIlisp.vim by Larry Clapp
"
if exists("g:VimArc_loaded")
  finish
else
  let g:VimArc_loaded = 1
endif

let g:VimArc_scratch = $HOME . "/.vimarc_scratch"
let s:pipe_name = $HOME . '/arc-wiki/.vimarc-pipe'
exe "new" g:VimArc_scratch
set syntax=lisp
set buftype=nowrite
set bufhidden=hide
set nobuflisted
set noswapfile
hide

function! VimArc_goto_pos( pos )
  let mx = '\(\f\+\)|\(\d\+\),\(\d\+\),\(\d\+\)'
  let bufname = substitute( a:pos, mx, '\1', '' )
  let l_top = substitute( a:pos, mx, '\2', '' )
  let l_cur = substitute( a:pos, mx, '\3', '' )
  let c_cur = substitute( a:pos, mx, '\4', '' )

  exe "hide bu" bufname
  exe "normal! " . l_top . "Gzt" . l_cur . "G" . c_cur . "|"
endfunction


" destroys contents of VimArc_scratch buffer
function! VimArc_send_to_lisp( sexp )
  if a:sexp == ''
    return
  endif

  let p = VimArc_get_pos()

  " goto VimArc_scratch, delete it, put s-exp, write it to lisp
  exe "hide bu" g:VimArc_scratch
  exe "%d"
  normal! 1G

  " tried append() -- doesn't work the way I need it to
  let old_l = @l
  let @l = a:sexp
  normal! "lP
  let @l = old_l

  exe 'w >>' s:pipe_name

  call VimArc_goto_pos( p )
endfunction

function! VimArc_yank( motion )
  let value = ''

  let p = VimArc_get_pos()
  silent! exec 'normal!' a:motion
  let new_p = VimArc_get_pos()

  " did we move?
  if p != new_p
      " go back
      silent! exec 'normal!' a:motion

      let old_l = @l
      exec 'normal! "ly' . a:motion
      let value = @l
      let @l = old_l
  endif

  call VimArc_goto_pos( p )

  return( value )
endfunction

function! VimArc_get_pos()
  " what buffer are we in?
  let bufname = bufname( "%" )

  " get current position
  let c_cur = virtcol( "." )
  let l_cur = line( "." )
  normal! H
  let l_top = line( "." )

  let pos = bufname . "|" . l_top . "," . l_cur . "," . c_cur

  " go back
  exe "normal! " l_cur . "G" . c_cur . "|"

  return( pos )
endfunction

function! VimArc_eval_defun_lisp()
  let p = VimArc_get_pos()

  silent! exec "normal! 99[("
  call VimArc_send_to_lisp( VimArc_yank( "%" ) )

  " fix cursor position, in case of error below
  call VimArc_goto_pos( p )
endfunction


nmap <C-Enter> :silent! call VimArc_eval_defun_lisp()<cr>
nmap <C-S-Enter> :silent! call VimArc_send_to_lisp( "(load \"" . expand( "%:p" ) . "\")\n")<cr>
