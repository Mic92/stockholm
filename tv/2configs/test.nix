{ config, lib, pkgs, ... }:

with lib;

let
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

  su-test = pkgs.execveBin "su-test" rec {
    filename = "/var/setuid-wrappers/sudo";
    argv = ["sudo" "-u" "test" "-i"];
  };

  test-shell = pkgs.execve "test-shell" rec {
    filename = "${pkgs.bash}/bin/bash";
    argv = ["sh" "--noprofile" "-l"];
    envp.ENV = pkgs.writeText "test-env" ''
      ${shell.cat "Hello, `$(j0w\nd0g!)`!\\o/\n"} >&2
    '';
  };

in out
