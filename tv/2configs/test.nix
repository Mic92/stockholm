{ config, lib, pkgs, ... }:

with import ../4lib { inherit lib pkgs; };

let
  tvpkgs = import ../5pkgs { inherit lib pkgs; };

  out = {
    security.sudo.extraConfig = ''
      tv ALL=(test) NOPASSWD: ALL
    '';
    users.extraUsers.test = {
      shell = "${test-shell}";
    };
  };

  test-shell = tvpkgs.execve "test-shell" rec {
    filename = "${pkgs.bash}/bin/bash";
    argv = ["sh" "--noprofile" "-l"];
    envp.ENV = pkgs.writeText "test-env" ''
      ${shell.cat "Hello, `$(j0w\nd0g!)`!\\o/\n"} >&2
    '';
  };

in out
