{ config, lib, pkgs, ... }:

with import ../../../lib/pure.nix { inherit lib; };
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "radioactive-2017-06-01";
    src = pkgs.fetchgit {
      url = "https://github.com/makefu/nagios-radioactiveathome-plugins/";
      rev = "955f614";
      sha256 = "0ql6npl3n6shvij0ly6a52yjmf7dc31c5x29y927k9lvp8ygin20";
    };
    buildInputs = [
      (pkgs.python3.withPackages (pythonPackages: with pythonPackages; [
        docopt
        requests
        python
      ]))
    ];
    installPhase = ''
      install -m755 -D add_many_points.py  $out/bin/radioactive-add-many
    '';
  };
in {
  systemd.services.radioactive = {
    description = "radioactive";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "nobody"; # TODO separate user
      ExecStart = "${pkg}/bin/radioactive-add-many loop 60";
      PrivateTmp = true;
      PermissionsStartOnly = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
