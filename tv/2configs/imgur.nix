with import <stockholm/lib>;
{ config, pkgs, ... }: {

  services.nginx.virtualHosts."ni.r" = {
    locations."/image" = {
      extraConfig = /* nginx */ ''
        client_max_body_size 20M;

        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        proxy_pass http://127.0.0.1:${toString config.krebs.htgen.imgur.port};
        proxy_pass_header Server;
      '';
    };
  };

  krebs.htgen.imgur = {
    port = 7771;
    scriptFile = "${pkgs.htgen-imgur}/bin/htgen-imgur";
  };
}
