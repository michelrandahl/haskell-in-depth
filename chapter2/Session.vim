let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Code/Haskell/HaskellInDepth/code/chapter2
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +8 app/Main.hs
badd +0 term://.//31845:/nix/store/6kxhv6s36p5l3jylxzwvqn4qm3fjkb63-bash-interactive-4.4-p23/bin/bash
badd +0 src/Chapter2.hs
badd +3 dirs.txt
badd +3 turns.txt
badd +29 chapter2.cabal
badd +1 shell.nix
badd +30 default.nix
argglobal
%argdel
edit app/Main.hs
set splitbelow splitright
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe '1resize ' . ((&lines * 36 + 27) / 55)
exe '2resize ' . ((&lines * 16 + 27) / 55)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 8 - ((7 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
8
normal! 02|
wincmd w
argglobal
if bufexists("term://.//31845:/nix/store/6kxhv6s36p5l3jylxzwvqn4qm3fjkb63-bash-interactive-4.4-p23/bin/bash") | buffer term://.//31845:/nix/store/6kxhv6s36p5l3jylxzwvqn4qm3fjkb63-bash-interactive-4.4-p23/bin/bash | else | edit term://.//31845:/nix/store/6kxhv6s36p5l3jylxzwvqn4qm3fjkb63-bash-interactive-4.4-p23/bin/bash | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 443 - ((15 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
443
normal! 063|
wincmd w
2wincmd w
exe '1resize ' . ((&lines * 36 + 27) / 55)
exe '2resize ' . ((&lines * 16 + 27) / 55)
tabnext 1
if exists('s:wipebuf') && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 winminheight=1 winminwidth=1 shortmess=filnxtToOF
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
