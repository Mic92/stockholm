{ pkgs, ... }:

{
  imports = [
    { users = import <secrets/users.nix>; }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = [
            (pkgs.lib.readFile <pubkeys/tv_wu.ssh.pub>)
          ];
        };
        tv = {
          uid = 1337;
          group = "users";
          home = "/home/tv";
          createHome = true;
          useDefaultShell = true;
          extraGroups = [
            "audio"
            "video"
            "wheel"
          ];
          openssh.authorizedKeys.keys = [
            (pkgs.lib.readFile <pubkeys/tv_wu.ssh.pub>)
          ];
        };
      };
    }
  ];

  users.defaultUserShell = "/run/current-system/sw/bin/bash";
  users.mutableUsers = false;

  security.setuidPrograms = [
    "sendmail"  # for sudo
  ];

  security.sudo.extraConfig = ''
    Defaults mailto="tv@wu.retiolum"
  '';
}
