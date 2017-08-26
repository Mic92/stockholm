{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "muell_caller-2017-06-01";
    src = pkgs.fetchgit {
      url = "https://github.com/shackspace/muell_caller/";
      rev = "bbd4009";
      sha256 = "1bfnfl2vdh0p5wzyz5p48qh04vvsg2445avg86fzhzragx25fqv0";
    };
    buildInputs = [
      (pkgs.python3.withPackages (pythonPackages: with pythonPackages; [
        docopt
        requests
        paramiko
        python
      ]))
    ];
    installPhase = ''
      install -m755 -D call.py  $out/bin/call-muell
    '';
  };
  cfg = "${toString <secrets>}/tell.json";
in {
  systemd.services.call_muell = {
    description = "call muell";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "nobody"; # TODO separate user
      ExecStartPre = pkgs.writeDash "call-muell-pre" ''
        cp ${cfg} /tmp/tell.json
        chown nobody /tmp/tell.json
      '';
      ExecStart = "${pkg}/bin/call-muell --cfg /tmp/tell.json --mode mpd loop 60";
      Restart = "always";
      PrivateTmp = true;
      PermissionsStartOnly = true;
    };
  };
}
