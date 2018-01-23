{ config, pkgs, ... }:

with import <stockholm/lib>;
let
  customPlugins.vim-javascript = pkgs.vimUtils.buildVimPlugin {
    name = "vim-javascript";
    src = pkgs.fetchFromGitHub {
      owner = "pangloss";
      repo = "vim-javascript";
      rev = "1.2.5.1";
      sha256 = "08l7ricd3j5h2bj9i566byh39v9n5wj5mj75f2c8a5dsc732b2k7";
    };
  };
   customPlugins.vim-jsx = pkgs.vimUtils.buildVimPlugin {
     name = "vim-jsx";
     src = pkgs.fetchFromGitHub {
       owner = "mxw";
       repo = "vim-jsx";
       rev = "5b968dfa512c57c38ad7fe420f3e8ab75a73949a";
       sha256 = "1z3yhhbmbzfw68qjzyvpbmlyv2a1p814sy5q2knn04kcl30vx94a";
     };
   };
in {
  environment.systemPackages = [
    (pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.customRC = let
        colorscheme = ''colorscheme molokai'';
        setStatements = ''
          set autowrite
          set clipboard=unnamedplus
          set nocompatible
          set path+=**
          set ruler
          set undodir=$HOME/.vim/undo  "directory where the undo files will be stored
          set undofile                 "turn on the feature
          set wildignore+=*.o,*.class,*.hi,*.dyn_hi,*.dyn_o
          set wildmenu
          set listchars=trail:¶
        '';
        remapStatements = ''
          imap jk <Esc>
          map gr :GoRun<Enter>         " Map gr to execute go run
          map nf :NERDTreeToggle<CR>
          nnoremap <C-TAB> <c-w><c-w>
          nnoremap <S-TAB> :bnext<CR>
          noremap x "_x
          vmap v v
        '';
        settingsForGo = ''
          let g:go_decls_includes = "func,type"
          let g:go_def_mode = 'godef'
          let g:go_fmt_command = "goimports"
          let g:go_highlight_extra_types = 1
          let g:go_highlight_fields = 1
          let g:go_highlight_functions = 1
          let g:go_highlight_methods = 1
          let g:go_highlight_types = 1
          let g:go_list_type = "quickfix"
          let g:go_metalinter_autosave = 1
          let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
          let g:go_snippet_case_type = "camelcase"
          let g:go_test_timeout = '10s'
          let g:jsx_ext_required = 0
          let g:molokai_original = 1
          let g:rehash256 = 1
        '';

      in ''
        ${colorscheme}
        ${remapStatements}
        ${setStatements}
        ${settingsForGo}
        " I dont know what this line is about
        autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
      '';
       vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
       vimrcConfig.vam.pluginDictionaries = [
         { names = [
            "ctrlp"
            "easymotion"
            "molokai"
            "nerdtree"
            "snipmate"
            "surround"
            "Syntastic"
            "undotree"
           ];
         }
         { names = [ "vim-addon-nix" ]; ft_regex = "^nix\$"; }
         { names = [ "vim-go" ]; ft_regex = "^go\$"; } # wanted: nsf/gocode
         { names = [ "vim-javascript" ]; ft_regex = "^js\$"; }
         { names = [ "vim-jsx" ]; ft_regex = "^js\$"; }
       ];
    })
  ];

  # set up the directories up if they are not there.
# Needs to be changed.
#  vim = let
#    dirs = {
#      backupdir = "$HOME/.cache/vim/backup";
#      swapdir   = "$HOME/.cache/vim/swap";
#      undodir   = "$HOME/.cache/vim/undo";
#    };
#    files = {
#      viminfo   = "$HOME/.cache/vim/info";
#    };
#
#    mkdirs = let
#      dirOf = s: let out = concatStringsSep "/" (init (splitString "/" s));
#                 in assert out != ""; out;
#      alldirs = attrValues dirs ++ map dirOf (attrValues files);
#    in unique (sort lessThan alldirs);
#  in
#    pkgs.symlinkJoin {
#      name = "vim";
#      paths = [
#        (pkgs.writeDashBin "vim" ''
#          set -efu
#          (umask 0077; exec ${pkgs.coreutils}/bin/mkdir -p ${toString mkdirs})
#          exec ${pkgs.vim}/bin/vim "$@"
#        '')
#        pkgs.vim
#      ];
#    };

}
