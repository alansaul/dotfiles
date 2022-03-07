set nocompatible
filetype indent plugin on | syn on
set hidden

" Auto install vim
fun! SetupPlug()
    " Set location of plugin packages
    "let addons_base = expand('$HOME') . '/dotfiles/vim_temp'
    let addons_base = expand('$HOME') . '/.vim'
    " May not be necessary
    exec 'set runtimepath+='.addons_base.'/plug.vim'

    " If no plugins are installed, automatically install plug.vim to
    " ~/.vim/autoload/
    if empty(glob(addons_base . '/plugged'))
      silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    endif

    call plug#begin(expand(addons_base) . '/plugged')

    Plug 'micha/vim-colors-solarized'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-fugitive'
    Plug 'preservim/nerdcommenter'
    Plug 'tmhedberg/SimpylFold'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'psf/black', { 'tag': '19.10b0' }
    
    "Plug 'Shougo/echodoc.vim'
    "Plug 'powerline/powerline'
    "
    " Install coc.vim
    " Fist make sure nodejs is installed
    " curl -sL install-node.now.sh | sh
    " Or
    " sudo apt install nodejs
    " sudo apt install npm
    " make sure yarn is also installed
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    "Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
    ":CocInstall coc-python
    " If you change python environment, need to set new interpreter
    " :CocCommand python.setInterpreter
    call plug#end()

    " Then install all packages (blocking), pipe into quit and then
    " resources the $VIMRC
    if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
      autocmd VimEnter * PlugInstall --sync | q | source $MYVIMRC
    endif
endf
call SetupPlug()

"call deoplete#custom#source('ale', 'rank', 999)

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
set history=10000

"Completion
"Set wildmenu which allows for command line completion
set wildmenu
set wildmode=list:longest,full
set completeopt+=longest,menu,preview

set colorcolumn=100

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
set viminfo='10,\"100,:20,%
if !has('nvim')
    set viminfo+=,n~/.vim/viminfo
endif

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
func! ToggleNumbers()
    "nu -> nonu -> rnu
    if exists('&relativenumber')
        if (&relativenumber == 1)
            set number
        elseif (&number == 1)
            set nonumber
        else
            set relativenumber
        endif
    else
        setl nu!
    endif
endfunc
noremap <silent> <C-N><C-N> :call ToggleNumbers()<CR>

"Plugin settings {
set laststatus=2 " always show the status line
"
" ale
" ale linting properties
"let g:ale_linters = {
           "\   'python': ['pylint', 'mypy'],
           "\}
"let g:ale_python_pylint_options = '--rcfile $HOME/Code/main/Build_System/linting/pylint.cfg --rcfile $HOME/Code/main/Build_System/linting/pylint-extra-rules-for-production-code.cfg'
"let g:ale_python_mypy_options = '--ignore-missing-imports'


" coc.nvim
" Mostly grabbed from https://github.com/neoclide/coc.nvim/#example-vim-configuration

set statusline^=%{coc#status()}  " set status line to one assigned by coc

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
"
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')
"
call coc#config(
\'python', {
\   'jediEnabled': v:false,
\   'linting.pep8Args': ['--max-line-length=100']
\})
call coc#config(
\'signature', {
\   'target': 'float',
\   'hideOnTextChange': v:false,
\   'preferShownAbove': v:false
\})
call coc#config(
\'suggest', {
\   'autoTrigger': 'none'
\})
call coc#config(
\'coc', {
\   'preferences.hoverTarget': 'float',
\   'preferences.jumpCommand': 'tabe'
\})

"{
    ""python.jediEnabled": true,
    ""suggest.autoTrigger": "none",
    ""signature.hideOnTextChange": false,
    ""signature.preferShownAbove": false,
    ""signature.target": "float",
    ""coc.preferences.hoverTarget": "float",
    ""diagnostic.displayByAle": false,
    ""coc.preferences.jumpCommand": "tabe"
"}

"
"\   'python.pythonPath': expand('$HOME') . '/dotfiles/python_int'

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
"if has('patch8.1.1068')
""  " Use `complete_info` if your (Neo)Vim version supports it.
""  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
"else
""  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"endif

nmap <expr> <leader>t <Plug>(coc-float-hide)

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

xmap <leader>A  <Plug>(coc-codeaction)
nmap <leader>A  <Plug>(coc-codeaction)

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>=  <Plug>(coc-format-selected)
nmap <leader>=  <Plug>(coc-format-selected)

"NERDTREE
" Setup nerd tree shortcut to see directory listings
noremap <leader>n :NERDTreeToggle<CR>  

"CtrlP
let g:ctrlp_map = '<leader>f'
nnoremap <leader>F :CtrlPBuffer<cr>
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc     " Linux/MacOSX

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

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

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
"au CursorMovedI * if pumvisible() == 0|pclose|endif
"au InsertLeave * if pumvisible() == 0|pclose|endif

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
set foldopen-=block  " don't open folds with paragraph jump, { or }

" Fix tmux's mappings for arrow keys
"noremap OA <up>
"noremap OB <down>
"noremap OC <right>
"noremap OD <left>
"set t_ku=OA
"set t_kd=OB
"set t_kr=OC
"set t_kl=OD
" noremap OA <up>
" noremap OB <down>
" noremap OC <right>
" noremap OD <left>

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

nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" Escape remap! Finally committed
inoremap jj <esc>
"Forget about <esc> we want to get used to jj
"inoremap <esc> <nop>
