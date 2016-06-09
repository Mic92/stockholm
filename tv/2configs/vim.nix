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

  extra-runtimepath = concatMapStringsSep "," (pkg: "${pkg.rtp}") [
    pkgs.vimPlugins.undotree
    (pkgs.vimUtils.buildVimPlugin {
      name = "file-line-1.0";
      src = pkgs.fetchgit {
        url = git://github.com/bogado/file-line;
        rev = "refs/tags/1.0";
        sha256 = "0z47zq9rqh06ny0q8lpcdsraf3lyzn9xvb59nywnarf3nxrk6hx0";
      };
    })
    ((rtp: rtp // { inherit rtp; }) (pkgs.writeTextFile (let
      name = "hack";
      version = "1.0";
    in {
      name = "vim-color-${name}-${version}";
      destination = "/colors/${name}.vim";
      text = /* vim */ ''
        set background=dark
        hi clear
        if exists("syntax_on")
          syntax clear
        endif

        let colors_name = ${toJSON name}

        hi Normal   ctermbg=235
        hi Comment  ctermfg=240

        hi Tabstop        ctermbg=16
        hi TrailingSpace  ctermbg=88
      '';
    })))
  ];

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
    colorscheme hack
    syntax on

    au Syntax * syn match Tabstop containedin=ALL /\t\+/
            \ | syn match TrailingSpace containedin=ALL /\s\+$/

    au BufRead,BufNewFile *.hs so ${hs.vim}

    au BufRead,BufNewFile *.nix so ${nix.vim}

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

  hs.vim = pkgs.writeText "hs.vim" ''
    syn region String start=+\[[[:alnum:]]*|+ end=+|]+
  '';

  nix.vim = pkgs.writeText "nix.vim" ''
    setf nix
    set isk=@,48-57,_,192-255,-,'

    syn match NixCode /./

    " Ref <nix/src/libexpr/lexer.l>
    syn match NixINT   /\<[0-9]\+\>/
    syn match NixPATH  /[a-zA-Z0-9\.\_\-\+]*\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
    syn match NixHPATH /\~\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
    syn match NixSPATH /<[a-zA-Z0-9\.\_\-\+]\+\(\/[a-zA-Z0-9\.\_\-\+]\+\)*>/
    syn match NixURI   /[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']\+/

    syn match NixString /"\([^\\"]\|\\.\)*"/
    syn match NixCommentMatch /\(^\|\s\)#.*/
    syn region NixCommentRegion start="/\*" end="\*/"

    hi NixCode ctermfg=034
    hi NixData ctermfg=040

    hi link NixComment Comment
    hi link NixCommentMatch NixComment
    hi link NixCommentRegion NixComment
    hi link NixINT NixData
    hi link NixPATH NixData
    hi link NixHPATH NixData
    hi link NixSPATH NixData
    hi link NixURI NixData
    hi link NixString NixData

    hi link NixEnter NixCode
    hi link NixExit NixData
    hi link NixQuote NixData
    hi link NixQuote2 NixQuote
    hi link NixQuote3 NixQuote

    syn cluster NixSubLangs contains=NONE

    ${concatStringsSep "\n" (mapAttrsToList (name: { start ? null }: let
    in /* vim */ ''
      syn include @${name}Syntax syntax/${name}.vim
      syn region ${name}Block
        \ matchgroup=NixExit
        \ start="\(/\* ${name} \*/${optionalString (start != null) ''\|${start}''}\) '''"
        \ skip="'''\('\|[$]\|\\[nrt]\)"
        \ end="'''"
        \ contains=@${name}Syntax
      syn cluster NixSubLangs add=${name}Block,@${name}Syntax
      unlet b:current_syntax
    '') {
      haskell = {};
      sh.start = ''write\(Ba\|Da\)sh[^ ]* *\"[^\"]*\"'';
      vim.start = ''write[^ ]* *\"\(\([^\"]*\.\)\?vimrc\|[^\"]*\.vim\)\"'';
    })}

    syn region NixBlock
      \ matchgroup=NixEnter
      \ start="[$]{"
      \ end="}"
      \ contains=TOP
      \ containedin=@NixSubLangs

    syn region NixBlockHack
      \ start="{"
      \ end="}"
      \ contains=TOP

    syn match NixQuote  "'''[$]"he=e-1  contained containedin=@NixSubLangs
    syn match NixQuote2 "''''"he=s+1    contained containedin=@NixSubLangs
    syn match NixQuote3 "'''\\[nrt]"    contained containedin=@NixSubLangs

    let b:current_syntax = "nix"
  '';
in
out
