{ pkgs, ... }:
with import <stockholm/lib>;
let
  versionOlderThan = v:
    compareVersions
      (versions.majorMinor version)
      (versions.majorMinor v)
      == -1;

  enable =
    versionOlderThan "19.03";
    warning = ''
      Using custom services.nscd.config because
      https://github.com/NixOS/nixpkgs/pull/50316
    '';
in
  optionalAttrs enable (trace warning {
    services.nscd.enable = mkForce true;
    services.nscd.config = mkForce (readFile (pkgs.fetchurl {
      url = https://raw.githubusercontent.com/arianvp/nixpkgs/1d5f4cb/nixos/modules/services/system/nscd.conf;
      sha256 = "1jlddk38lyynjn51zx3xi1nc29ahajyh0qg48qbq6dqlsrn3wxqs";
    }));
  })

