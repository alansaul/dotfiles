"Alans ~/.vimrc script, uses VAM (thanks MarcWeber) to handle all plugins!
set nocompatible
filetype indent plugin on | syn on
set hidden

" let's copy paste some lines from documentation
fun! SetupVAM()
    let addons_base = expand('$HOME') . '/dotfiles/vim/vim-addons'
    exec 'set runtimepath+='.addons_base.'/vim-addon-manager'

    if !isdirectory(addons_base)
        exec '!p='.shellescape(addons_base).'; mkdir -p "$p" && cd "$p" && git clone git://github.com/MarcWeber/vim-addon-manager.git'
    endif

    let g:vim_addon_manager = {}
    let g:vim_addon_manager['plugin_sources'] = {}
    let g:vim_addon_manager['plugin_sources']['snippets'] = { 'type' : 'git', 'url': 'git://github.com/alansaul/ultisnips-snippets.git' }
    let g:vim_addon_manager['plugin_sources']['jedi-vim'] = { 'type' : 'git', 'url': 'git://github.com/davidhalter/jedi-vim.git' }
    "let g:vim_addon_manager['plugin_sources']['tma-multiple-cursors'] = { 'type' : 'git', 'url': 'git://github.com/terryma/vim-multiple-cursors.git' }

    "let g:vim_addon_manager['plugin_sources']['snippets'] = { 'type' : 'git', 'url': 'git://github.com/scrooloose/snipmate-snippets.git' } << Using my snippets for now as scroolooses has the wrong directory structure to work with upstream VAM, also mine includes lazily loading functions
    call vam#ActivateAddons(['Solarized', 'blackboard', 'desert256', 'molokai', 'wombat256', 'Railscasts_Theme_GUI256color', 'xoria256', 'ctrlp', 'AutoTag', 'The_NERD_tree', 'endwise', 'surround', 'UltiSnips', 'snippets', 'YankRing', 'The_NERD_Commenter', 'Python-mode-klen', 'fugitive', 'jedi-vim', 'Syntastic', 'AutomaticLaTeXPlugin', 'powerline', 'Supertab'], {'auto_install': 1})
    "'LaTeX-Suite_aka_Vim-LaTeX', 

endf
call SetupVAM()


"Neat directory initalization, ripped form
"https://github.com/spf13/spf13-vim/blob/master/.vimrc
function! InitializeDirectories()
    let separator = "."
    let parent = $HOME
    let prefix = '.vim'
    let dir_list = {
                \ 'backup': 'backupdir',
                \ 'swap': 'directory' }

    if has('persistent_undo')
        let dir_list['undo'] = 'undodir'
    endif

    for [dirname, settingname] in items(dir_list)
        let directory = parent . '/' . prefix . dirname . "/"
        if exists("*mkdir")
            if !isdirectory(directory)
                call mkdir(directory)
            endif
        endif
        if !isdirectory(directory)
            echo "Warning: Unable to create backup directory: " . directory
            echo "Try: mkdir -p " . directory
        else
            let directory = substitute(directory, " ", "\\\\ ", "g")
            exec "set " . settingname . "=" . directory
        endif
    endfor
endfunction
call InitializeDirectories()

"Make it so commands are run on the shell that it was loaded with (zsh)
"set shell=$SHELL\ -i

" General {
" try to detect filetypes
filetype on
filetype plugin on 
filetype indent on

"Tabs
set tabstop=4
set shiftwidth=4
set smarttab "Delete by tabspaces (so <BS> deletes a whole tab)
set expandtab "Expand a tabstop character to shiftwidths
set autoindent

set undofile "Allow files to be undone when reopened by loading undos from undodir
set undoreload=10000 "Number of lines to undo
set ttyfast "Fast keys
set splitbelow "Forces new windows to be below the current one
set splitright "Forces new windows to be to the right of the current one
"Make the split windows even again when a window is resized (check this works
"on multiple systems)
au VimResized * exe "normal! \<c-w>=" 
"For osx (atleast on iTerm2) this allows the mouse to be used to scroll and
"select lines
set mouse=a
"
"set how many lines of history vim has to remember
set history=1000

"Completion
"Set wildmenu which allows for command line completion
set wildmenu
set wildmode=list:longest,full
set completeopt+=longest,menu,preview

"English spell checker by typing :set spell and z= for suggestion
":set spell spelllang=en

" Weirdly the backspace stops working on existing text without this
set backspace=indent,eol,start

"Timeout before accepting that this is only the first keycode (i.e. it is
"<C-L> not <C-L><C-L>
set timeoutlen=500

" Tell vim to remember certain things when we exit
"  '10 : marks will be remembered for up to 10 previously edited files
"  "100 : will save up to 100 lines for each register
"  :20 : up to 20 lines of command-line history will be remembered
"  % : saves and restores the buffer list
"  n... : where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

set scrolloff=3 "Minimum number of lines to have around the cursor showing (so you can't be typing on the bottom line)
"}

" Vim UI {
set cursorline " highlight current line
set shortmess=aOstT " shortens messages to avoid 'press a key' prompt
set showmatch " show matching brackets
"}

"Search {
" Remove search highlighting with <C-L>
"CONFLICT WITH SWITCH BUFFER
noremap <C-L><C-L> <Esc>:syntax sync fromstart<CR>:let @/=""<CR>
nnoremap <C-L> :nohls<CR>
" Clear the search term so (n and p no longer search again
"map <C-L><C-L> :let @/=""<CR>
set hlsearch " Highlight searched terms
set incsearch " BUT do highlight as you type you search phrase
set ignorecase "Ignore case normally
set smartcase "But if it has a capital in it, then pay attention to case

" Mappings {
"Set up map leader
let mapleader = ';'

" Keep search matches in the middle of the window and pulse the line when
" moving to them.
nnoremap n nzzzv
nnoremap N Nzzzv
"}

syntax on
if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
    set t_Co=256
endif
set background=dark
"let g:solarized_termcolors=256
"let g:solarized_visibility="low"
colorscheme solarized

" Line Return {{{

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END


"Set up relative line numbering instead of absolute as its useful to perform
"actions to multiple lines
"Make <C-N><C-N> toggle between line numberings relative absolute and none
noremap <silent> <C-N><C-N> :call ToggleNumbers()<CR>
func! ToggleNumbers()
    "nu -> nonu -> rnu
    if exists('&rnu')
        if &rnu == 1
            set nu
        elseif &nu == 1
            set nonu
        else
            set rnu
        endif
    else
        setl nu!
    endif

endfunc

"Plugin settings {
"Syntastic (And status line, taken from Steve Losh

"let g:syntastic_python_checkers = [''] "This is a bit of a hack, this should be default? Syntastic will spaz out if you dont have pyflakes (which you should) on the PATH

"set statusline=%f    " Path.
"set statusline+=%m   " Modified flag.
"set statusline+=%r   " Readonly flag.
"set statusline+=%w   " Preview window flag.
"set statusline+=\ [%{getcwd()}]          " current dir
"set statusline+=%{fugitive#statusline()} "  Git Hotness

"set statusline+=\    " Space.

"set statusline+=%#redbar#                " Highlight the following as a warning.
"set statusline+=%{SyntasticStatuslineFlag()} " Syntastic errors.
"set statusline+=%*                           " Reset highlighting.

"set statusline+=%=   " Right align.


"" Line and column position and counts.
"set statusline+=\ (line\ %l\/%L,\ col\ %03c)

"set statusline+=%#warningmsg# "enable flags in status bar
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_enable_signs=1 "enable signs in side bar
"let g:syntastic_auto_loc_list=2
set laststatus=2

"Supertab
let g:SuperTabDefaultCompletionType = "context"

"PYMODE
"imap <C-A> <C-R>=RopeCodeAssistInsertMode()<CR>
let g:pymode_rope = 0
let g:pymode_folding = 0
let g:pymode_lint = 1
let g:pymode_lint_write = 1
let g:pymode_lint_signs = 1
let g:pymode_lint_checker = "pylint"
let g:pymode_lint_ignore="E231,E225,E501,C0301"
let g:pymode_run_key = '<leader>p' " Key for run python code
let g:pymode_breakpoint_cmd = "import ipdb; ipdb.set_trace()  # XXX BREAKPOINT"

"
"Jedi
let g:jedi#completions_command = "<C-A>"
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 0

"TASK LIST
" Toggle task list (type \td to see a list of TODO:'s etc"
"noremap <leader>td <Plug>TaskList

" TagList Settings {
"let Tlist_Auto_Open=0 " let the tag list open automagically
"let Tlist_Compact_Format = 1 " show small menu
"let Tlist_Ctags_Cmd = 'ctags' " location of ctags
"let Tlist_Enable_Fold_Column = 0 " do show folding tree
"let Tlist_Exist_OnlyWindow = 1 " if you are the last, kill yourself
"let Tlist_File_Fold_Auto_Close = 0 " fold closed other trees
"let Tlist_Sort_Type = "name" " order by 
"let Tlist_Use_Right_Window = 1 " split to the right side of the screen
"let Tlist_WinWidth = 40 " 40 cols wide, so i can (almost always read my
"   functions)

"PROJECT
" Add recently accessed projects menu (project plugin)
"set viminfo^=!

"NERDTREE
" Setup nerd tree shortcut to see directory listings
noremap <leader>n :NERDTreeToggle<CR>  

"CtrlP
let g:ctrlp_map = '<leader>f'
nnoremap <leader>F :CtrlPBuffer<cr>
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc     " Linux/MacOSX

"EXUBERANT TAGS
" Remake ctags with F5
"noremap <silent> <F6> :!ctags -R --exclude=.svn --exclude=.git --exclude=log .<CR>
"noremap <F7> :!ctags -R --exclude=.svn --exclude=.git --exclude=log .<CR>
"Set up tag toggle mapping
"nnoremap <leader>t :TagbarToggle<CR>
"Tell it where to find tags
"autocmd BufWritePost *
      "\ if filereadable('tags') |
      "\   call system('ctags -R .') |
      "\ endif
"set tags=./tags,tags;$HOME

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
"

"Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "vim-addons/snippets/snippets"]
let g:ultisnips_python_style="sphinx"

"vim-ipython
"Run python console, python qtconsole or python kernel in terminal
"Type :IPython to bind vim to the console
"<C-S> to pass a line to ipython \d to analyse whats under the cursor, can
"execute code in the console and it has access to the <C-S> stuff. Finally
"<C-X><C-U> autocompletes
"<leader>d to get docs for function name under word (import needs to have been
"ipython loaded)

" vim-Latex alternative things
" LaTeX (rubber) macro for compiling
"nnoremap <leader>ll :w<CR>:!rubber --pdf --warn all %<CR>

" View PDF macro; '%:r' is current file's root (base) name.
"nnoremap <leader>v :!mupdf %:r.pdf &<CR><CR>

let g:atp_Compiler = "bash"
let b:atp_TexCompiler = "pdflatex"
let b:atp_Viewer = "evince"
let g:atp_ProgressBar=1
nnoremap <leader>L :Tex<CR>
let g:conceallevel=2
let g:atp_imap_leader_2="##"
let g:atp_imap_leader_3="]"
let g:atp_imap_leader_1="#"

"vim-latex
" Set colorscheme, enable conceal (except for
" subscripts/superscripts), and match conceal
" highlight to colorscheme
" let g:tex_conceal= 'adgm'
" hi Conceal guibg=brblack guifg=brcyan
"hi Conceal cterm=NONE ctermbg=NONE ctermfg=white
"au VimEnter * hi Conceal cterm=NONE ctermbg=NONE ctermfg=white
"au VimEnter * set conceallevel=2
"au ColorScheme * hi! link Conceal Normal
"set cole=2
"let g:tex_conceal="asgm"

"let g:tex_flavor = "latex"
"let g:Tex_DefaultTargetFormat = 'pdf'
 
"let g:Tex_CompileRule_dvi = 'latex --interaction=nonstopmode $*'
"let g:Tex_CompileRule_ps = 'dvips -Pwww -o $*.ps $*.dvi'
"let g:Tex_CompileRule_pspdf = 'ps2pdf $*.ps'
"let g:Tex_CompileRule_dvipdf = 'dvipdfm $*.dvi'
"let g:Tex_CompileRule_pdf = 'pdflatex -synctex=1 --interaction=nonstopmode $*'
"nnoremap <leader>r :w<CR>:!rubber --pdf --warn all %<CR>
 
"let g:Tex_FormatDependency_ps  = 'dvi,ps'
"let g:Tex_FormatDependency_pspdf = 'dvi,ps,pspdf'
"let g:Tex_FormatDependency_dvipdf = 'dvi,dvipdf'

"autocmd BufNewFile,BufRead *.tex set spell

"nnoremap <leader>kk :silent call Tex_RunLaTeX()<CR>
"set shortmess+=A "Don't show warning about swap files!
"let g:Tex_ShowErrorContext = 0 "Dont show log file
"let g:Tex_GotoError = 0 "Dont jump to quickfix window showing errors

"au BufWritePost *.tex silent call Tex_RunLaTeX()
"au BufWritePost *.tex silent !pkill -USR1 xdvi.bin
 
" let g:Tex_IgnoredWarnings ='
"       \"Underfull\n".
"       \"Overfull\n".
"       \"specifier changed to\n".
"       \"You have requested\n".
"       \"Missing number, treated as zero.\n".
"       \"There were undefined references\n".
"       \"Citation %.%# undefined\n".
"       \"\oval, \circle, or \line size unavailable\n"' 


" }


" Auto commands {
" Python {
au BufRead,BufNewFile *.py set tabstop=4
au BufRead,BufNewFile *.py set shiftwidth=4
au BufRead,BufNewFile *.py set softtabstop=4 
"}
" Ruby {
" ruby standard 2 spaces, always
au bufread,bufnewfile *.rb,*.rhtml set tabstop=2 
au bufread,bufnewfile *.rb,*.rhtml set shiftwidth=2 
au bufread,bufnewfile *.rb,*.rhtml set softtabstop=2 
"If its an erb file, give html and ruby snippets
au BufNewFile,BufRead *.html.erb set filetype=eruby.html
"}
" Haskell {
au bufread,bufnewfile *.hs, set tabstop=8 
au bufread,bufnewfile *.hs, set shiftwidth=8 
au bufread,bufnewfile *.hs, set softtabstop=8 
"}

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
au CursorMovedI * if pumvisible() == 0|pclose|endif
au InsertLeave * if pumvisible() == 0|pclose|endif

" Java {
" First call eclimd from the Eclipse folder, then :PingEclim
" }
" }


" Fix D and Y
nnoremap D d$
nnoremap Y y$

"Fix J so it doesnt drop down a line when you join
nnoremap J mzJ`z

" Make it so j and k navigate up and down regardless of whether 2 lines is
" actually 1!
nnoremap j gj
nnoremap k gk

noremap q: :q
" Save a read only file using sudo tee %
cnoreabbrev <expr> w!!
            \((getcmdtype() == ':' && getcmdline() == 'w!!')
            \?('!sudo tee % >/dev/null'):('w!!'))

":command TODO :noautocmd vimgrep /TODO/jg **/* | copen
":command FIXME :noautocmd vimgrep /FIXME/jg **/* | copen
":command TODOrb :silent! noautocmd vimgrep /TODO/jg **/*.rb **/*.feature **/*.html **/*.haml **/*.scss **/*.css | copen
":command FIXMErb :silent! noautocmd vimgrep /FIXME/jg **/*.rb **/*.feature **/*.html **/*.haml **/*.scss **/*.css | copen


"Clear the quickfix (useful when you've done a TODOrb and want to get rid of
"the results!)
":command Clearqf :cex [] 

"Close quickfix if only window open
aug QFClose
  au!
  au WinEnter * if winnr('$') == 1 && getbufvar(winbufnr(winnr()), "&buftype") == "quickfix"|q|endif
aug END


" Easier buffer navigation
noremap <leader>h <C-w>h
noremap <leader>j <C-w>j
noremap <leader>k <C-w>k
noremap <leader>l <C-w>l
nnoremap <c-left> 5<c-w>>
nnoremap <c-right> 5<c-w><

"Folds - LEARN TO USE THEM
" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" Fix tmux's mappings for arrow keys
"noremap OA <up>
"noremap OB <down>
"noremap OC <right>
"noremap OD <left>
"set t_ku=OA
"set t_kd=OB
"set t_kr=OC
"set t_kl=OD
noremap OA <up>
noremap OB <down>
noremap OC <right>
noremap OD <left>

" Command to run current script by typing ;e
"map ;p :w<CR>:exe ":!python " . getreg("%") . "" <CR>

"Add a new line
nnoremap <CR> O<ESC>j
"But not for quickfix windows! (in qf the enter should go to the error!)
autocmd FileType qf nnoremap <buffer> <CR> <CR>

"Remap increment number from <C-A> (which is used in screen) to <C-I>
nnoremap <C-I> <C-A>

" DISABLE ARROW KEYS < Just comment this out if you wan't arrow keys to work
" again
" ==================
"
" Insert Mode
"inoremap <Up> <NOP>
"inoremap <Down> <NOP>
"inoremap <Left> <NOP>
"inoremap <Right> <NOP>
" Normal Mode
"noremap <Up> <NOP>
"noremap <Down> <NOP>
"noremap <Left> <NOP>
"noremap <Right> <NOP>

" Navigate Omnicomplete with jk
"inoremap <expr> i ((pumvisible())?("\<C-n>"):("i"))
"inoremap <expr> o ((pumvisible())?("\<C-p>"):("o"))

"Set up remaps for markers as ' is easier to press. Normally ' goes to the
"line of the marker, and ` goes to the column and line of the marker, here I
"am swapping them because it is easier to press ' and I would usually want
"the effect of `
nnoremap ' `
nnoremap ` '

" ROT13 - fun
"noremap <leader>r ggVGg?
" }
"

nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" Escape remap! Finally committed
inoremap jj <esc>
"Forget about <esc> we want to get used to jj
"inoremap <esc> <nop>
