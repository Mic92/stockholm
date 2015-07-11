{ lib, ... }:

with lib;

{
  options = {
    nixpkgs.url = mkOption {
      type = types.str;
      description = "URL of the nixpkgs repository.";
    };
    nixpkgs.rev = mkOption {
      type = types.str;
      default = "origin/master";
      description = "Revision of the remote repository.";
    };
    nixpkgs.dirty = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If nixpkgs.url is a local path, then use that as it is.
        TODO this break if URL is not a local path.
      '';
    };
  };
}
