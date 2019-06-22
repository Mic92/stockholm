{ pkgs, config, ... }: let
  #unstable = import <nixos-unstable> { };
in

{
  environment.variables = {
    EDITOR = ["nvim"];
  };

  nixpkgs.config.packageOverrides = pkgs: with pkgs;{
    neovim_custom = neovim.override {
      configure = {
        customRC = builtins.readFile ./neovimrc;

        packages.myVimPackage = with pkgs.vimPlugins;
        {
          # loaded on launch
          start = [
            nerdtree # file manager
            commentary # comment stuff out based on language
            fugitive # full git integration
            vim-airline-themes # lean & mean status/tabline
            vim-airline # status bar
            gitgutter # git diff in the gutter (sign column)
            vim-trailing-whitespace # trailing whitspaces in red
            tagbar # F3 function overview
            syntastic # Fallback to singlethreaded but huge syntax support
            ReplaceWithRegister # For better copying/replacing
            polyglot # Language pack
            vim-indent-guides # for displaying indent levels
            ale # threaded language client
            vim-go # go linting
            deoplete-go # go autocompletion completion
            deoplete-nvim # general autocompletion
            molokai # color scheme
          ];

          # manually loadable by calling `:packadd $plugin-name`
          opt = [];
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [
    ctags
    neovim_custom
    jq # For fixing json files
    xxd # .bin files will be displayed with xxd
    shellcheck # Shell linting
    ansible-lint # Ansible linting
    unzip # To vim into unzipped files
    nodePackages.jsonlint # json linting
    #python36Packages.python-language-server # python linting
    #python36Packages.pyls-mypy # Python static type checker
    #python36Packages.black # Python code formatter
    #python37Packages.yamllint # For linting yaml files
    #python37Packages.libxml2 # For fixing yaml files
    cquery # C/C++ support
    clang-tools # C++ fixer
  ];

  fonts = {
    fonts = with pkgs; [
      font-awesome_5
    ];
  };

}

