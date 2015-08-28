{ config, lib, pkgs, ... }:

with import ../4lib { inherit lib pkgs; };

let
  tvpkgs = import ../5pkgs { inherit lib pkgs; };

  out = {
    environment.systemPackages = [
      su-test
    ];
    security.sudo.extraConfig = ''
      tv ALL=(test) NOPASSWD: ALL
    '';
    users.extraUsers.test = {
      shell = "${test-shell}";
    };
  };

  su-test = tvpkgs.execveBin "su-test" rec {
    filename = "/var/setuid-wrappers/sudo";
    argv = ["sudo" "-u" "test" "-i"];
  };

  test-shell = tvpkgs.execve "test-shell" rec {
    filename = "${pkgs.bash}/bin/bash";
    argv = ["sh" "--noprofile" "-l"];
    envp.ENV = pkgs.writeText "test-env" ''
      ${shell.cat "Hello, `$(j0w\nd0g!)`!\\o/\n"} >&2
    '';
  };

in out
