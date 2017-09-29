with import <stockholm/lib>;
{ config, pkgs, ... }: {

  options.tv.nixpkgs-overlays = mkOption {
    apply = src:
      pkgs.runCommand "nixpkgs-overlays" {} ''
        mkdir $out
        ${concatStringsSep "\n" (mapAttrsToList (name: path:
          "ln -s ${shell.escape path} $out/${shell.escape name}"
        ) src)}
      '' // {
        inherit src;
      };
    type = types.attrsOf types.absolute-pathname;
  };

  config = {
    tv.nixpkgs-overlays = {
      krebs = mkDefault "/var/src/stockholm/krebs/5pkgs";
      tv = mkDefault "/var/src/stockholm/tv/5pkgs";
    };
  };
}
