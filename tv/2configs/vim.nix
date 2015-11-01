{ lib, pkgs, ... }:

with lib;
let
  out = {
    environment.systemPackages = [
      vim'
    ];

    environment.variables.EDITOR = mkForce "vim";
  };

  runtimepath = concatStringsSep "," [
    "$HOME/.vim" # TODO get rid if this and incorporate everything from wu:~tv/.vim
    "${pkgs.vimPlugins.undotree}/share/vim-plugins/undotree"
    #"${tv-vim}/share/vim"
  ];

  vim-industry-colors = pkgs.writeTextFile rec {
    name = "vim-industry-colors";
    destination = "/colors/${name}";
    text = ''
      " Vim color file
      " Maintainer:	Shian Lee 
      " Last Change:	2014 Mar 6 (for vim 7.4)
      " Remark:	"industry" stands for 'industrial' color scheme. In industrial
      "		HMI (Human-Machine-Interface) programming, using a standard color
      "               scheme is mandatory in many cases (in traffic-lights for example): 
      "               LIGHT_RED is	    'Warning' 
      "               LIGHT_YELLOW is	    'Attention' 
      "               LIGHT_GREEN is	    'Normal' 
      "               LIGHT_MAGENTA is    'Warning-Attention' (light RED-YELLOW)
      "               LIGHT_CYAN is	    'Attention-Normal'  (light YELLOW-GREEN).
      "               BLACK is	    Dark-High-Contrast Background for maximum safety.
      "               BLUE is		    Shade of BLACK (not supposed to get attention).
      "
      "               Industrial color scheme is by nature clear, safe and productive. 
      "               Yet, depends on the file type's syntax, it might appear incorrect. 

      " Reset to dark background, then reset everything to defaults:
      set background=dark
      highlight clear
      if exists("syntax_on")
          syntax reset
      endif

      let colors_name = "industry"

      " First set Normal to regular white on black text colors:
      hi Normal ctermfg=LightGray ctermbg=Black guifg=#dddddd	guibg=Black

      " Syntax highlighting (other color-groups using default, see :help group-name):
      hi Comment    cterm=NONE ctermfg=DarkCyan    	gui=NONE guifg=#00aaaa   	 
      hi Constant   cterm=NONE ctermfg=LightCyan   	gui=NONE guifg=#00ffff   	
      hi Identifier cterm=NONE ctermfg=LightMagenta   gui=NONE guifg=#ff00ff   
      hi Function   cterm=NONE ctermfg=LightGreen   	gui=NONE guifg=#00ff00   	
      hi Statement  cterm=NONE ctermfg=White	     	gui=bold guifg=#ffffff	     	
      hi PreProc    cterm=NONE ctermfg=Yellow		gui=NONE guifg=#ffff00 	
      hi Type	      cterm=NONE ctermfg=LightGreen	gui=bold guifg=#00ff00 		
      hi Special    cterm=NONE ctermfg=LightRed    	gui=NONE guifg=#ff0000    	
      hi Delimiter  cterm=NONE ctermfg=Yellow    	gui=NONE guifg=#ffff00    	
    '';
  };

  vim' = pkgs.writeScriptBin "vim" ''
    #! /bin/sh
    set -eu
    mkdir -p "$HOME"/.vim/backup
    mkdir -p "$HOME"/.vim/cache
    mkdir -p "$HOME"/.vim/undo
    exec ${pkgs.vim}/bin/vim -u ${vimrc} "$@"
  '';

  vimrc = pkgs.writeText "vimrc" ''
    set nocompatible

    set autoindent
    set backspace=indent,eol,start
    set backup
    set backupdir=$HOME/.vim/backup/
    set directory=$HOME/.vim/cache//
    set hlsearch
    set incsearch
    set mouse=a
    set pastetoggle=<INS>
    set runtimepath=${runtimepath}
    set shortmess+=I
    set showcmd
    set showmatch
    set ttimeoutlen=0
    set undodir=$HOME/.vim/undo
    set undofile
    set undolevels=1000000
    set undoreload=1000000
    set viminfo='20,<1000,s100,h,n$HOME/.vim/cache/info
    set visualbell
    set wildignore+=*.o,*.class,*.hi,*.dyn_hi,*.dyn_o
    set wildmenu
    set wildmode=longest,full

    filetype plugin indent on

    "colorscheme industry # TODO
    syntax on

    cmap w!! w!sudo tee % >/dev/null

    nmap <esc>q :buffer
    nmap <M-q> :buffer

    cnoremap <C-A> <Home>

    noremap  <C-c> :q<cr>

    nnoremap <esc>[5^  :tabp<cr>
    nnoremap <esc>[6^  :tabn<cr>
    nnoremap <esc>[5@  :tabm -1<cr>
    nnoremap <esc>[6@  :tabm +1<cr>

    nnoremap <f1> :tabp<cr>
    nnoremap <f2> :tabn<cr>
    inoremap <f1> <esc>:tabp<cr>
    inoremap <f2> <esc>:tabn<cr>

    " <C-{Up,Down,Right,Left>
    noremap <esc>Oa <nop> | noremap! <esc>Oa <nop>
    noremap <esc>Ob <nop> | noremap! <esc>Ob <nop>
    noremap <esc>Oc <nop> | noremap! <esc>Oc <nop>
    noremap <esc>Od <nop> | noremap! <esc>Od <nop>
    " <[C]S-{Up,Down,Right,Left>
    noremap <esc>[a <nop> | noremap! <esc>[a <nop>
    noremap <esc>[b <nop> | noremap! <esc>[b <nop>
    noremap <esc>[c <nop> | noremap! <esc>[c <nop>
    noremap <esc>[d <nop> | noremap! <esc>[d <nop>
    vnoremap u <nop>
  '';

  # "7.4.335" -> "74"
  #majmin = x: concatStrings (take 2 (splitString "." x));
in
out
#https://github.com/mbbill/undotree
