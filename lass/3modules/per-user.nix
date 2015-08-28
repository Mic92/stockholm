{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  cfg = config.lass.per-user;

  out = {
    options.lass.per-user = api;
    config = imp;
  };

  api = mkOption {
    type = with types; attrsOf (submodule {
      options = {
        packages = mkOption {
          type = listOf path;
          default = [];
        };
      };
    });
    default = {};
  };

  imp = {
    #
    # TODO only shellInit and use well-known paths
    #
    environment.shellInit = ''
      if test -e ${user-profiles}/"$LOGNAME"; then
        . ${user-profiles}/"$LOGNAME"
      fi
    '';
    environment.interactiveShellInit = ''
      if test -e ${user-profiles}/"$LOGNAME"; then
        . ${user-profiles}/"$LOGNAME"
      fi
    '';
    environment.profileRelativeEnvVars.PATH = mkForce [ "/bin" ];
  };

  user-profiles = pkgs.runCommand "user-profiles" {} ''
    mkdir $out
    ${concatStrings (mapAttrsToList (logname: { packages, ... }: ''
      cat > $out/${logname} <<\EOF
      ${optionalString (length packages > 0) (
        let path = makeSearchPath "bin" packages; in
        ''export PATH="$PATH":${escapeShellArg path}''
      )}
      EOF
    '') cfg)}
  '';

in out
