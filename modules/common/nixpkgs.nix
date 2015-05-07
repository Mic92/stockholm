{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    nixpkgs.url = mkOption {
      type = types.string;
      description = "url of the remote repo";
    };
    nixpkgs.rev= mkOption {
      type = types.string;
      description = "revision of the remote repo";
    };
  };
}
