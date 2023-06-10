{ config, lib, pkgs, ... }:

with import ../../../lib/pure.nix { inherit lib; };
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "muell_caller-2017-06-01";
    src = pkgs.fetchgit {
      url = "https://github.com/shackspace/muell_caller/";
      rev = "ee4e499";
      sha256 = "0q1v07q633sbqg4wkgf0zya2bnqrikpyjhzp05iwn2vcs8rvsi3k";
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
      PrivateTmp = true;
      PermissionsStartOnly = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
