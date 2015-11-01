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
