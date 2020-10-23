{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  services.nginx.virtualHosts.paste = {
    serverAliases = [ "p.r" ];
    locations."/".extraConfig = ''
      client_max_body_size 4G;
      proxy_set_header Host $host;
      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.paste.port};
    '';
    locations."/image".extraConfig = /* nginx */ ''
      client_max_body_size 40M;

      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;

      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.imgur.port};
      proxy_pass_header Server;
    '';
  };
  services.nginx.virtualHosts."p.krebsco.de" = {
    enableACME = true;
    addSSL = true;
    serverAliases = [ "p.krebsco.de" ];
    locations."/".extraConfig = ''
      if ($request_method != GET) {
        return 403;
      }
      proxy_set_header Host $host;
      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.paste.port};
    '';
    locations."/image".extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;

      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.imgur.port};
      proxy_pass_header Server;
    '';
  };

  krebs.htgen.paste = {
    port = 9081;
    script = toString [
      "PATH=${makeBinPath [
        pkgs.nix
        pkgs.file
      ]}:$PATH"
      "STATEDIR=$HOME"
      ". ${pkgs.htgen}/examples/paste"
    ];
  };
  krebs.htgen.imgur = {
    port = 7771;
    script = /* sh */ ''
      (. ${pkgs.htgen-imgur}/bin/htgen-imgur)
    '';
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT";}
  ];
}
