{ config, ... }:

{
  imports = [ ../../3modules/tv/consul.nix ];
  tv.consul = rec {
    enable = true;

    inherit (config.tv.identity) self;
    inherit (self) dc;

    server = true;

    hosts = with config.tv.identity.hosts; [
      # TODO get this list automatically from each host where tv.consul.enable is true
      cd
      mkdir
      nomic
      rmdir
      #wu
    ];
  };
}
