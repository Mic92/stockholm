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
      description = "Revision of the remote repository.";
    };
  };
}
