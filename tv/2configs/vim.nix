{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  out = {
    environment.systemPackages = [
      vim
    ];

    environment.etc.vimrc.source = vimrc;

    environment.variables.EDITOR = mkForce "vim";
    environment.variables.VIMINIT = ":so /etc/vimrc";
  };

  extra-runtimepath = let
    inherit (pkgs.vimUtils) buildVimPlugin rtpPath;
    fromVimPlugins = pkgs: concatStringsSep ","
      (mapAttrsToList (name: pkg: "${pkg}/${rtpPath}/${name}") pkgs);
  in fromVimPlugins {
    inherit (pkgs.vimPlugins) undotree;
    file-line = buildVimPlugin {
      name = "file-line-1.0";
      src = pkgs.fetchgit {
        url = git://github.com/bogado/file-line;
        rev = "refs/tags/1.0";
        sha256 = "0z47zq9rqh06ny0q8lpcdsraf3lyzn9xvb59nywnarf3nxrk6hx0";
      };
    };
  };

  dirs = {
    backupdir = "$HOME/.cache/vim/backup";
    swapdir   = "$HOME/.cache/vim/swap";
    undodir   = "$HOME/.cache/vim/undo";
  };
  files = {
    viminfo   = "$HOME/.cache/vim/info";
  };

  mkdirs = let
    dirOf = s: let out = concatStringsSep "/" (init (splitString "/" s));
               in assert out != ""; out;
    alldirs = attrValues dirs ++ map dirOf (attrValues files);
  in unique (sort lessThan alldirs);

  vim = pkgs.writeDashBin "vim" ''
    set -efu
    (umask 0077; exec ${pkgs.coreutils}/bin/mkdir -p ${toString mkdirs})
    exec ${pkgs.vim}/bin/vim "$@"
  '';

  vimrc = pkgs.writeText "vimrc" ''
    set nocompatible

    set autoindent
    set backspace=indent,eol,start
    set backup
    set backupdir=${dirs.backupdir}/
    set directory=${dirs.swapdir}//
    set hlsearch
    set incsearch
    set mouse=a
    set noruler
    set pastetoggle=<INS>
    set runtimepath=${extra-runtimepath},$VIMRUNTIME
    set shortmess+=I
    set showcmd
    set showmatch
    set ttimeoutlen=0
    set undodir=${dirs.undodir}
    set undofile
    set undolevels=1000000
    set undoreload=1000000
    set viminfo='20,<1000,s100,h,n${files.viminfo}
    set visualbell
    set wildignore+=*.o,*.class,*.hi,*.dyn_hi,*.dyn_o
    set wildmenu
    set wildmode=longest,full

    set et ts=2 sts=2 sw=2

    filetype plugin indent on

    set t_Co=256
    colorscheme industry
    syntax on

    au Syntax * syn match Tabstop containedin=ALL /\t\+/
            \ | hi Tabstop ctermbg=16
            \ | syn match TrailingSpace containedin=ALL /\s\+$/
            \ | hi TrailingSpace ctermbg=88
            \ | hi Normal ctermfg=White

    au BufRead,BufNewFile *.hs so ${pkgs.writeText "hs.vim" ''
      syn region String start=+\[[[:alnum:]]*|+ end=+|]+
    ''}

    au BufRead,BufNewFile *.nix so ${pkgs.writeText "nix.vim" ''
      setf nix
      set isk=@,48-57,_,192-255,-,'

      " Ref <nix/src/libexpr/lexer.l>
      syn match INT   /\<[0-9]\+\>/
      syn match PATH  /[a-zA-Z0-9\.\_\-\+]*\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
      syn match HPATH /\~\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
      syn match SPATH /<[a-zA-Z0-9\.\_\-\+]\+\(\/[a-zA-Z0-9\.\_\-\+]\+\)*>/
      syn match URI   /[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']\+/
      hi link INT Constant
      hi link PATH Constant
      hi link HPATH Constant
      hi link SPATH Constant
      hi link URI Constant

      syn match String /"\([^\\"]\|\\.\)*"/
      syn match Comment /\(^\|\s\)#.*/

      let b:current_syntax = "nix"
    ''}

    au BufRead,BufNewFile /dev/shm/* set nobackup nowritebackup noswapfile

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
in
out
