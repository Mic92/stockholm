{ config, lib, pkgs, ... }: let
  mud = pkgs.writers.writeDashBin "mud" ''
    set -efux
    MUD_NICKNAME=''${MUD_NICKNAME:-$(head -1 /dev/urandom | md5sum | cut -c -2)}
    MUD_SERVER=''${MUD_SERVER:-127.0.0.1}
    MUD_PORT=''${MUD_PORT:-8080}

    if $(${pkgs.netcat-openbsd}/bin/nc -z "$MUD_SERVER" "$MUD_PORT"); then
      ${nvim}/bin/nvim \
        +"let g:instant_username = \"$MUD_NICKNAME\"" \
        +":InstantJoinSession $MUD_SERVER $MUD_PORT" \
        "$@"
    else
      ${nvim}/bin/nvim \
        +"let g:instant_username = \"$MUD_NICKNAME\"" \
        +":InstantStartServer $MUD_SERVER $MUD_PORT" \
        +":InstantStartSession $MUD_SERVER $MUD_PORT" \
        "$@"
    fi
  '';
  nvim = pkgs.neovim.override {
    # vimAlias = true;
    configure = {
      customRC = vimrc;
      packages.myPlugins = with pkgs.vimPlugins; {
        start = [
          vim-surround # Shortcuts for setting () {} etc.
          # coc-nvim coc-git coc-highlight coc-python coc-rls coc-vetur coc-vimtex coc-yaml coc-html coc-json # auto completion
          vim-nix # nix highlight
          fzf-vim # fuzzy finder through vim
          nerdtree # file structure inside nvim
          rainbow # Color parenthesis
          customPlugins.hack-color
          customPlugins.instant
        ];
        opt = [];
      };
    };
  };
  vimrc = /* vim */ ''
    set nocompatible

    set autoindent
    set backspace=indent,eol,start
    set backup
    set backupdir=$HOME/.cache/nvim/backup/
    set directory=$HOME/.cache/nvim/swap"//
    set hlsearch
    set incsearch
    set mouse=a
    set ruler
    set pastetoggle=<INS>
    set shortmess+=I
    set showcmd
    set showmatch
    set ttimeoutlen=0
    set undodir=$HOME/.cache/nvim/undo
    set undofile
    set undolevels=1000000
    set undoreload=1000000
    set viminfo='20,<1000,s100,h,n$HOME/.cache/nvim/info
    set visualbell
    set wildignore+=*.o,*.class,*.hi,*.dyn_hi,*.dyn_o
    set wildmenu
    set wildmode=longest,full

    set title
    set titleold=
    set titlestring=(vim)\ %t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}

    set et ts=2 sts=2 sw=2

    filetype plugin indent on

    set t_Co=256
    colorscheme hack
    syntax on

    au Syntax * syn match Garbage containedin=ALL /\s\+$/
            \ | syn match TabStop containedin=ALL /\t\+/
            \ | syn keyword Todo containedin=ALL TODO

    au BufRead,BufNewFile /dev/shm/* set nobackup nowritebackup noswapfile

    nmap <esc>q :buffer 
    nmap <M-q> :buffer 

    cnoremap <C-A> <Home>

    noremap  <C-c> :q<cr>
    vnoremap < <gv
    vnoremap > >gv

    nnoremap <f1> :tabp<cr>
    nnoremap <f2> :tabn<cr>
    inoremap <f1> <esc>:tabp<cr>
    inoremap <f2> <esc>:tabn<cr>
  '';
  customPlugins = {
    instant = pkgs.vimUtils.buildVimPlugin {
      name = "instant";
      src = pkgs.fetchFromGitHub {
        owner = "jbyuki";
        repo = "instant.nvim";
        rev = "c02d72267b12130609b7ad39b76cf7f4a3bc9554";
        sha256 = "sha256-7Pr2Au/oGKp5kMXuLsQY4BK5Wny9L1EBdXtyS5EaZPI=";
      };
    };
    hack-color = (rtp: rtp // { inherit rtp; }) (pkgs.writeTextFile (let
      name = "hack";
    in {
      name = "vim-color-${name}-1.0.2";
      destination = "/colors/${name}.vim";
      text = /* vim */ ''
        set background=dark
        hi clear
        if exists("syntax_on")
          syntax clear
        endif

        let colors_name = ${builtins.toJSON name}

        hi Normal       ctermbg=016
        hi Comment      ctermfg=255
        hi Constant     ctermfg=229
        hi Identifier   ctermfg=123
        hi Function     ctermfg=041
        hi Statement    ctermfg=167
        hi PreProc      ctermfg=167
        hi Type         ctermfg=046
        hi Delimiter    ctermfg=251
        hi Special      ctermfg=146

        hi Garbage      ctermbg=124
        hi TabStop      ctermbg=020
        hi NBSP         ctermbg=056
        hi NarrowNBSP   ctermbg=097
        hi Todo         ctermfg=174 ctermbg=NONE

        hi NixCode      ctermfg=190
        hi NixData      ctermfg=149
        hi NixQuote     ctermfg=119

        hi diffNewFile  ctermfg=207
        hi diffFile     ctermfg=207
        hi diffLine     ctermfg=207
        hi diffSubname  ctermfg=207
        hi diffAdded    ctermfg=010
        hi diffRemoved  ctermfg=009
      '';
    }));
  };
in {
  users.users.mud = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = with config.krebs.users; [
      lass.pubkey
      makefu.pubkey
      kmein-kabsa.pubkey
      kmein-manakish.pubkey
      tv.pubkey
    ];
    packages = with pkgs; [
      tmux
      (pkgs.writers.writeDashBin "instant_server" ''
        find ${customPlugins.instant}
        find ${customPlugins.instant.src}
      '')
      mud
    ];
  };
}
