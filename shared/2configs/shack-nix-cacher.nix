{ pkgs, lib, ... }:

{
  krebs.nginx = {
    enable = lib.mkDefault true;
    servers = {
      apt-cacher-ng = {
        server-names = [ "acng.shack" ];
        locations = lib.singleton (lib.nameValuePair "/" ''
          proxy_set_header   Host $host;
          proxy_set_header   X-Real-IP          $remote_addr;
          proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_pass http://localhost:3142/;
        '');
      };
    };
  };

  krebs.apt-cacher-ng = {
    enable = true;
    port = 3142;
    bindAddress = "localhost";
    cacheExpiration = 30;
  };
}
