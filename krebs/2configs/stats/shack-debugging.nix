{ config, lib, pkgs, ... }:

# TODO: krebs.collectd.plugins
with import ../../../lib/pure.nix { inherit lib; };
let
  connect-time-cfg = with pkgs; writeText "collectd-connect-time.conf" ''
    LoadPlugin python
    <Plugin python>
      ModulePath "${collectd-connect-time}/lib/${python.libPrefix}/site-packages/"
      Import "collectd_connect_time"
      <Module collectd_connect_time>
        target "localhost:22" "google.com" "google.de" "gum.krebsco.de" "10.42.0.1:22" "heise.de" "t-online.de""10.0.1.3" "10.0.0.3:22" "10.0.0.4:22"
        interval 10
      </Module>
    </Plugin>
  '';
  graphite-cfg = pkgs.writeText "collectd-graphite.conf" ''
    LoadPlugin write_graphite
    <Plugin "write_graphite">
      <Carbon>
        Host "graphite.shack"
        Port "2003"
        Prefix "retiolum."
        EscapeCharacter "_"
        StoreRates false
        AlwaysAppendDS false
      </Carbon>
    </Plugin>
  '';
in {
  imports = [ ];

  nixpkgs.config.packageOverrides = pkgs: with pkgs; {
    collectd = pkgs.collectd.override { python= pkgs.python; };
  };
  services.collectd = {
    enable = true;
    include = [ (toString connect-time-cfg) (toString graphite-cfg) ];
  };

}
