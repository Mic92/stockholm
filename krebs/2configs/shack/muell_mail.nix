{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/muell_mail";
      rev = "317370e3e98ce34da4ee615af7a80df7b519ab89";
      sha256 = "sha256:02mywm37n0v4icgy474wwkavb7vad93bvkigvz1cqn7fbg4ldc8k";
    }) {};
    home = "/var/lib/muell_mail";
    cfg = toString <secrets/shack/muell_mail.js>;
in {
  users.users.muell_mail = {
    inherit home;
    createHome = true;
  };
  systemd.services.muell_mail = {
    description = "muell_mail";
    wantedBy = [ "multi-user.target" ];
    environment.CONFIG = "${home}/muell_mail.js";
    serviceConfig = {
      User = "muell_mail";
      ExecStartPre = pkgs.writeDash "muell_mail-pre" ''
        install -D -omuell_mail -m700 ${cfg} ${home}/muell_mail.js
      '';
      WorkingDirectory = home;
      PermissionsStartOnly = true;
      ExecStart = "${pkg}/bin/muell_mail";
      Restart = "always";
      PrivateTmp = true;
    };
  };
}
