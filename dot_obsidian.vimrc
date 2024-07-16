unmap <Space>
unmap ;

" doesn't seem to work
"let mapleader=";"

set clipboard=unnamed
inoremap jj <Esc>

exmap followLinkUnderCursor obcommand editor:follow-link
nmap ;gd :followLinkUnderCursor

exmap goback obcommand app:go-back
nmap ;dg :goback

" Emulate Tab Switching https://vimhelp.org/tabpage.txt.html#gt
" requires Pane Relief: https://github.com/pjeby/pane-relief
"exmap tabnext obcommand pane-relief:go-next
"nmap ;gt :tabnext
"exmap tabprev obcommand pane-relief:go-prev
"nmap ;gT :tabprev
" Same as CMD+\
"nmap ;g\ :tabnext

"Simpler version, but doesn't work with split panes
exmap tabnext obcommand workspace:next-tab
exmap tabprev obcommand workspace:previous-tab
nmap ;gt :tabnext
nmap ;gT :tabprev


" rename file
"exmap renameFile obcommand Obsidian-VimEx:file-rename-modal
"nmap ;gr :renameFile

exmap N obcommand workspace:new-tab
exmap n obcommand file-explorer:new-file-in-current-tab
exmap mv obcommand file-explorer:move-file
exmap e obcommand switcher:open

" mapping vs/hs as workspace split
exmap vs obcommand workspace:split-vertical
exmap sp obcommand workspace:split-horizontal
nmap ;sp :sp
nmap ;vs :vs

" window controls
exmap wq obcommand workspace:close
exmap q obcommand workspace:close

