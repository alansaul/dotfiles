My ~/.vimrc files and vim files (most of which will be loaded with VAM from the ~/.vimrc but I am keeping a backup regardless.

I'm hoping people will find it useful! It's mostly used for ruby editing.

Simply copy the .vimrc to ~/.vimrc and load up any file with vim (vim ~/tester.rb) and the plugins should be instantly installed using VAM (Great work by MarcWeber)

Eclim is not installed via VAM as this is not recommended (it is dependant on the set up of Eclipse). To use Eclim please visit their website: http://eclim.org/ and follow the instructions.

Snippets are loaded from my own snippets repository which is a fork of scroolooses and almost identical (as of 13/11/11), although the directory structure allows for snippets to be loaded with VAM without any hacks, and fits the upstream way of using snippets with snipMate. Some functions are also lazily loaded.
