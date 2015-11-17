{ pkgs, lib, ... }:

{
  krebs.nginx = {
    enable = lib.mkDefault true;
    servers = {
      drivedroid-repo = {
        server-names = [ "drivedroid.shack" ];
        # TODO: prepare this somehow
        locations = lib.singleton (lib.nameValuePair "/" ''
          root /var/srv/drivedroid;
          index main.json;
        '');
      };
    };
  };

}
