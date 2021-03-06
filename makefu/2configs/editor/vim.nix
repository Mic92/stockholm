{ config, pkgs, ... }:

{

  environment.systemPackages = [
    ((pkgs.vim_configurable.override { python = pkgs.python3; }).customize {
      name = "vim";
      vimrcConfig.customRC = builtins.readFile ./vimrc;
      vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
      vimrcConfig.vam.pluginDictionaries = [
        { names = [ "undotree"
        "YouCompleteMe"
        #"UltiSnips"
          "vim-better-whitespace" ]; }
        # vim-nix handles indentation better but does not perform sanity
        { names = [ "vim-addon-nix" ]; ft_regex = "^nix\$"; }
      ];

    })
  ];
}
