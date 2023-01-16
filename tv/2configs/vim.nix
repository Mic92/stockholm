with import ./lib;
{ config, pkgs, ... }: let {
  body = {
    environment.systemPackages = [
      vim-wrapper
    ];

    environment.etc.vimrc.source = vimrc;

    environment.variables.EDITOR = mkForce "vim";
    environment.variables.VIMINIT = ":so /etc/vimrc";
  };

  base-plugins = [
    pkgs.tv.vimPlugins.file-line
    pkgs.tv.vimPlugins.hack
    pkgs.vimPlugins.undotree
    (pkgs.tv.vim.makePlugin (pkgs.write "vim-tv-base" {
      "/ftplugin/haskell.vim".text = ''
        if exists("g:vim_tv_ftplugin_haskell_loaded")
          finish
        endif
        let g:vim_tv_ftplugin_haskell_loaded = 1

        setlocal iskeyword+='
      '';
    }))
  ];

  extra-plugins = [
    pkgs.tv.vimPlugins.elixir
    pkgs.tv.vimPlugins.fzf
    pkgs.tv.vimPlugins.jq
    pkgs.tv.vimPlugins.nix
    pkgs.tv.vimPlugins.showsyntax
    pkgs.tv.vimPlugins.tv
    pkgs.tv.vimPlugins.vim
    pkgs.vimPlugins.fzfWrapper
    pkgs.vimPlugins.vim-nftables
  ];

  dirs = {
    backupdir = "$HOME/.cache/vim/backup";
    swapdir   = "$HOME/.cache/vim/swap";
    undodir   = "$HOME/.cache/vim/undo";
  };
  files = {
    viminfo   = "$HOME/.cache/vim/info";
  };

  need-dirs = let
    dirOf = s: let out = concatStringsSep "/" (init (splitString "/" s));
               in assert out != ""; out;
    alldirs = attrValues dirs ++ map dirOf (attrValues files);
  in unique (sort lessThan alldirs);

  vim-wrapper = pkgs.symlinkJoin {
    name = "vim";
    paths = [
      (pkgs.writeDashBin "vim" ''
        set -efu
        export FZF_DEFAULT_COMMAND='${pkgs.ripgrep}/bin/rg --files'
        export PATH=$PATH:${makeBinPath [
          pkgs.fzf
          pkgs.ripgrep
        ]}
        (umask 0077; exec ${pkgs.coreutils}/bin/mkdir -p ${toString need-dirs})
        exec ${pkgs.vim}/bin/vim "$@"
      '')
      pkgs.vim
    ];
  };

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
    set runtimepath=${pkgs.tv.vim.makeRuntimePath base-plugins},$VIMRUNTIME
    set shortmess+=I
    set showcmd
    set showmatch
    set timeoutlen=0
    set ttimeoutlen=0
    set ttymouse=sgr
    set undodir=${dirs.undodir}
    set undofile
    set undolevels=1000000
    set undoreload=1000000
    set viminfo='20,<1000,s100,h,n${files.viminfo}
    set visualbell
    set wildignore+=*.o,*.class,*.hi,*.dyn_hi,*.dyn_o
    set wildmenu
    set wildmode=longest,full

      set runtimepath^=${pkgs.tv.vim.makeRuntimePath extra-plugins}
      syntax on

    set et ts=2 sts=2 sw=2

    filetype plugin indent on

    set t_Co=256
    colorscheme hack

    au Syntax * syn match Garbage containedin=ALL /\s\+$/
            \ | syn match TabStop containedin=ALL /\t\+/
            \ | syn keyword Todo containedin=ALL TODO

    au BufRead,BufNewFile *.nix set ft=nix

    au BufRead,BufNewFile /dev/shm/* set nobackup nowritebackup noswapfile

    cnoremap <C-A> <Home>

    noremap  <C-c> :q<cr>

    nnoremap <esc>[5^  :tabp<cr>
    nnoremap <esc>[6^  :tabn<cr>
    nnoremap <esc>[5@  :tabm -1<cr>
    nnoremap <esc>[6@  :tabm +1<cr>

    nnoremap <f1> :tabp<cr>
    nnoremap <f2> :tabn<cr>
    imap <f1> <esc><f1>
    imap <f2> <esc><f2>

    noremap <f3> :ShowSyntax<cr>

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

    " fzf
    nnoremap <esc>q :Buffers<cr>
    nnoremap <esc>f :Files<cr>
    nnoremap <esc>w :Rg<cr>

    " edit alternate buffer
    " For some reason neither putting <ctrl>6 nor <ctrl>^ works here...
    nnoremap <esc>a 

    if $TOUCHSCREEN == 1
      nnoremap <ScrollWheelUp> <C-y>
      nnoremap <ScrollWheelDown> <C-e>
      nnoremap <C-ScrollWheelUp> 3<C-y>
      nnoremap <C-ScrollWheelDown> 3<C-e>
      nnoremap <S-ScrollWheelUp> 3<C-y>
      nnoremap <S-ScrollWheelDown> 3<C-e>
      nnoremap <C-S-ScrollWheelUp> <PageUp>
      nnoremap <C-S-ScrollWheelDown> <PageDown>
    endif
  '';
}
