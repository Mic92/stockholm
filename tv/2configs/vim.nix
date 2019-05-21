{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let {
  body = {
    environment.systemPackages = [
      vim-wrapper
    ];

    environment.etc.vimrc.source = vimrc;

    environment.variables.EDITOR = mkForce "vim";
    environment.variables.VIMINIT = ":so /etc/vimrc";
  };

  extra-runtimepath = concatMapStringsSep "," (pkg: "${pkg.rtp}") [
    pkgs.tv.vimPlugins.elixir
    pkgs.tv.vimPlugins.file-line
    pkgs.tv.vimPlugins.fzf
    pkgs.tv.vimPlugins.hack
    pkgs.tv.vimPlugins.jq
    pkgs.vimPlugins.fzfWrapper
    pkgs.vimPlugins.undotree
    ((rtp: rtp // { inherit rtp; }) (pkgs.writeTextFile (let
      name = "vim";
    in {
      name = "vim-syntax-${name}-1.0.0";
      destination = "/syntax/${name}.vim";
      text = /* vim */ ''
        ${concatMapStringsSep "\n" (s: /* vim */ ''
          syn keyword vimColor${s} ${s}
            \ containedin=ALLBUT,vimComment,vimLineComment
          hi vimColor${s} ctermfg=${s}
        '') (map (i: lpad 3 "0" (toString i)) (range 0 255))}
      '';
    })))
    ((rtp: rtp // { inherit rtp; }) (pkgs.writeTextFile (let
      name = "showsyntax";
    in {
      name = "vim-plugin-${name}-1.0.0";
      destination = "/plugin/${name}.vim";
      text = /* vim */ ''
        if exists('g:loaded_showsyntax')
          finish
        endif
        let g:loaded_showsyntax = 0

        fu! ShowSyntax()
          let id = synID(line("."), col("."), 1)
          let name = synIDattr(id, "name")
          let transName = synIDattr(synIDtrans(id),"name")
          if name != transName
            let name .= " (" . transName . ")"
          endif
          echo "Syntax: " . name
        endfu

        command! -n=0 -bar ShowSyntax :call ShowSyntax()
      '';
    })))
    ((rtp: rtp // { inherit rtp; }) (pkgs.write "vim-tv" {
      #
      # Haskell
      #
      "/ftplugin/haskell.vim".text = ''
        if exists("g:vim_tv_ftplugin_haskell_loaded")
          finish
        endif
        let g:vim_tv_ftplugin_haskell_loaded = 1

        setlocal iskeyword+='
      '';
      #
      # TODO
      #
      "/ftdetect/todo.vim".text = ''
        au BufRead,BufNewFile TODO set ft=todo
      '';
      "/ftplugin/todo.vim".text = ''
        setlocal foldmethod=syntax
      '';
      "/syntax/todo.vim".text = ''
        syn match todoComment /#.*/

        syn match todoDate /^[1-9]\S*/
          \ nextgroup=todoSummary

        syn region todoSummary
          \ contained
          \ contains=todoTag
          \ start="." end="$\n"
          \ nextgroup=todoBlock

        syn match todoTag /\[[A-Za-z]\+\]/hs=s+1,he=e-1
          \ contained

        syn region todoBlock
          \ contained
          \ contains=Comment
          \ fold
          \ start="^[^1-9]" end="^[1-9]"re=s-1,he=s-1,me=s-1

        syn sync minlines=1000

        hi link todoComment Comment
        hi todoDate ctermfg=255
        hi todoSummary ctermfg=229
        hi todoBlock ctermfg=248
        hi todoTag ctermfg=217
      '';
    }))
    ((rtp: rtp // { inherit rtp; }) (pkgs.write "vim-syntax-nix-nested" {
      "/syntax/haskell.vim".text = ''
        syn region String start=+\[[[:alnum:]]*|+ end=+|]+

        hi link ConId Identifier
        hi link VarId Identifier
        hi link hsDelimiter Delimiter
      '';
      "/syntax/nix.vim".text = ''
        "" Quit when a (custom) syntax file was already loaded
        "if exists("b:current_syntax")
        "  finish
        "endif

        "setf nix

        " Ref <nix/src/libexpr/lexer.l>
        syn match NixID    /[a-zA-Z\_][a-zA-Z0-9\_\'\-]*/
        syn match NixINT   /\<[0-9]\+\>/
        syn match NixPATH  /[a-zA-Z0-9\.\_\-\+]*\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
        syn match NixHPATH /\~\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
        syn match NixSPATH /<[a-zA-Z0-9\.\_\-\+]\+\(\/[a-zA-Z0-9\.\_\-\+]\+\)*>/
        syn match NixURI   /[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']\+/
        syn region NixSTRING
          \ matchgroup=NixSTRING
          \ start='"'
          \ skip='\\"'
          \ end='"'
        syn region NixIND_STRING
          \ matchgroup=NixIND_STRING
          \ start="'''"
          \ skip="'''\('\|[$]\|\\[nrt]\)"
          \ end="'''"

        syn match NixOther /[-!+&<>|():/;=.,?\[\]*@]/

        syn match NixCommentMatch /\(^\|\s\)#.*/
        syn region NixCommentRegion start="/\*" end="\*/"

        hi link NixCode Statement
        hi link NixData Constant
        hi link NixComment Comment

        hi link NixCommentMatch NixComment
        hi link NixCommentRegion NixComment
        hi link NixID NixCode
        hi link NixINT NixData
        hi link NixPATH NixData
        hi link NixHPATH NixData
        hi link NixSPATH NixData
        hi link NixURI NixData
        hi link NixSTRING NixData
        hi link NixIND_STRING NixData

        hi link NixEnter NixCode
        hi link NixOther NixCode
        hi link NixQuote NixData

        syn cluster nix_has_dollar_curly contains=@nix_ind_strings,@nix_strings
        syn cluster nix_ind_strings contains=NixIND_STRING
        syn cluster nix_strings contains=NixSTRING

        ${concatStringsSep "\n" (mapAttrsToList (name: {
          extraStart ? null,
          lang ? name
        }:
        let
          startAlts = filter isString [
            ''/\* ${name} \*/''
            extraStart
          ];
          sigil = ''\(${concatStringsSep ''\|'' startAlts}\)[ \t\r\n]*'';
        in /* vim */ ''
          syn include @nix_${lang}_syntax syntax/${lang}.vim
          if exists("b:current_syntax")
            unlet b:current_syntax
          endif

          syn match nix_${lang}_sigil
            \ X${replaceStrings ["X"] ["\\X"] sigil}\ze\('''\|"\)X
            \ nextgroup=nix_${lang}_region_IND_STRING,nix_${lang}_region_STRING
            \ transparent

          syn region nix_${lang}_region_STRING
            \ matchgroup=NixSTRING
            \ start='"'
            \ skip='\\"'
            \ end='"'
            \ contained
            \ contains=@nix_${lang}_syntax
            \ transparent

          syn region nix_${lang}_region_IND_STRING
            \ matchgroup=NixIND_STRING
            \ start="'''"
            \ skip="'''\('\|[$]\|\\[nrt]\)"
            \ end="'''"
            \ contained
            \ contains=@nix_${lang}_syntax
            \ transparent

          syn cluster nix_ind_strings
            \ add=nix_${lang}_region_IND_STRING

          syn cluster nix_strings
            \ add=nix_${lang}_region_STRING

          " This is required because containedin isn't transitive.
          syn cluster nix_has_dollar_curly
            \ add=@nix_${lang}_syntax
        '') (let

          capitalize = s: let
            xs = stringToCharacters s;
          in
            toUpper (head xs) + concatStrings (tail xs);

          alts = xs: ''\(${concatStringsSep ''\|'' xs}\)'';
          def = k: ''${k}[ \t\r\n]*='';
          writer = k: ''write${k}[^ \t\r\n]*[ \t\r\n]*\("[^"]*"\|[a-z]\+\)'';

          writerExt = k: writerName ''[^"]*\.${k}'';
          writerName = k: ''write[^ \t\r\n]*[ \t\r\n]*"${k}"'';

        in {
          c = {};
          cabal = {};
          diff = {};
          haskell = {};
          jq.extraStart = alts [
            (writer "Jq")
            (writerExt "jq")
          ];
          javascript.extraStart = ''/\* js \*/'';
          lua = {};
          python.extraStart = ''/\* py \*/'';
          sed.extraStart = writer "Sed";
          sh.extraStart = let
            phases = [
              "unpack"
              "patch"
              "configure"
              "build"
              "check"
              "install"
              "fixup"
              "installCheck"
              "dist"
            ];
            shells = [
              "ash"
              "bash"
              "dash"
            ];
          in alts [
            (def "shellHook")
            (def "${alts phases}Phase")
            (def "${alts ["pre" "post"]}${alts (map capitalize phases)}")
            (writer (alts (map capitalize shells)))
          ];
          yaml = {};
          vim.extraStart = alts [
            (def ''"[^"]*\.vim"\.text'')
            (writerExt "vim")
            (writerName ''\([^"]*\.\)\?vimrc'')
          ];
          xdefaults = {};
          xmodmap = {};
        }))}

        " Clear syntax that interferes with nixINSIDE_DOLLAR_CURLY.
        syn clear shVarAssign

        syn region nixINSIDE_DOLLAR_CURLY
          \ matchgroup=NixEnter
          \ start="[$]{"
          \ end="}"
          \ contains=TOP
          \ containedin=@nix_has_dollar_curly
          \ transparent

        syn region nix_inside_curly
          \ matchgroup=NixEnter
          \ start="{"
          \ end="}"
          \ contains=TOP
          \ containedin=nixINSIDE_DOLLAR_CURLY,nix_inside_curly
          \ transparent

        syn match NixQuote /'''\(''$\|\\.\)/he=s+2
          \ containedin=@nix_ind_strings
          \ contained

        syn match NixQuote /'''\('\|\\.\)/he=s+1
          \ containedin=@nix_ind_strings
          \ contained

        syn match NixQuote /\\./he=s+1
          \ containedin=@nix_strings
          \ contained

        syn sync fromstart

        let b:current_syntax = "nix"

        set isk=@,48-57,_,192-255,-,'
      '';
      "/syntax/sed.vim".text = ''
        syn region sedBranch
          \ matchgroup=sedFunction start="T"
          \ matchgroup=sedSemicolon end=";\|$"
          \ contains=sedWhitespace
      '';
      "/syntax/xmodmap.vim".text = ''
        syn match xmodmapComment /^\s*!.*/
      '';
    }))
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
    set runtimepath=$VIMRUNTIME,${extra-runtimepath}
    set shortmess+=I
    set showcmd
    set showmatch
    set timeoutlen=0
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
    inoremap <f1> <esc>:tabp<cr>
    inoremap <f2> <esc>:tabn<cr>

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
    nnoremap <esc>q :Files<cr>
    nnoremap <esc>w :Rg<cr>

    " edit alternate buffer
    " For some reason neither putting <ctrl>6 nor <ctrl>^ works here...
    nnoremap <esc>a 
  '';
}
