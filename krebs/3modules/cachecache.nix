{ pkgs, config, lib, ... }:


# fork of https://gist.github.com/rycee/f495fc6cc4130f155e8b670609a1e57b
# related: https://github.com/nh2/nix-binary-cache-proxy

with lib;

let

  cfg = config.krebs.cachecache;

  nginxCfg = config.services.nginx;

  cacheFallbackConfig = {
    proxyPass = "$upstream_endpoint";
    extraConfig = ''
      # Default is HTTP/1, keepalive is only enabled in HTTP/1.1.
      proxy_http_version 1.1;

      # Remove the Connection header if the client sends it, it could
      # be "close" to close a keepalive connection
      proxy_set_header Connection "";

      # Needed for CloudFront.
      proxy_ssl_server_name on;

      proxy_set_header Host $proxy_host;
      proxy_cache nix_cache_cache;
      proxy_cache_valid 200 302 60m;
      proxy_cache_valid 404 1m;

      expires max;
      add_header Cache-Control $nix_cache_cache_header always;
    '';
  };

in

{
  options = {
    krebs.cachecache = {
      enable = mkEnableOption "Nix binary cache cache";

      virtualHost = mkOption {
        type = types.str;
        default = "nix-cache";
        description = ''
          Name of the nginx virtualhost to use and setup. If null, do
          not setup any virtualhost.
        '';
      };
      enableSSL = mkOption {
        type = types.bool;
        default = true;
        description = ''
          enable SSL via letsencrypt. Requires working dns resolution and open
          internet tls port.
        '';
      };

      resolver = mkOption {
        type = types.str;
        description = "Address of DNS resolver.";
        default = "8.8.8.8 ipv6=off";
        example = "127.0.0.1 ipv6=off";
      };

      cacheDir = mkOption {
        type = types.str;
        default = "/var/cache/nix-cache-cache";
        description = ''
          Where nginx should store cached data.
        '';
      };
      indexFile = mkOption {
        type = types.path;
        default = pkgs.writeText "myindex" "<html>hello world</html>";
        description = ''
          Path to index.html file.
        '';
      };

      maxSize = mkOption {
        type = types.str;
        default = "50g";
        description = "Maximum cache size.";
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];


    systemd.services.nginx.preStart = ''
      mkdir -p ${cfg.cacheDir} /srv/www/nix-cache-cache
      chmod 700 ${cfg.cacheDir} /srv/www/nix-cache-cache
      ln -fs ${cfg.indexFile} /srv/www/nix-cache-cache/index.html
      chown ${nginxCfg.user}:${nginxCfg.group} \
        ${cfg.cacheDir} /srv/www/nix-cache-cache
    '';

    services.nginx = {
      enable = true;

      appendHttpConfig = ''
        proxy_cache_path ${cfg.cacheDir}
          levels=1:2
          keys_zone=nix_cache_cache:100m
          max_size=${cfg.maxSize}
          inactive=365d
          use_temp_path=off;

        # Cache only success status codes; in particular we don't want
        # to cache 404s. See https://serverfault.com/a/690258/128321.
        map $status $nix_cache_cache_header {
          200     "public";
          302     "public";
          default "no-cache";
        }
      '';

      virtualHosts.${cfg.virtualHost} = {
        addSSL = cfg.enableSSL;
        enableACME = cfg.enableSSL;
        extraConfig = ''
          # Using a variable for the upstream endpoint to ensure that it is
          # resolved at runtime as opposed to once when the config file is loaded
          # and then cached forever (we don't want that):
          # see https://tenzer.dk/nginx-with-dynamic-upstreams/
          # This fixes errors like
          #
          #   nginx: [emerg] host not found in upstream "upstream.example.com"
          #
          # when the upstream host is not reachable for a short time when
          # nginx is started.
          resolver ${cfg.resolver} valid=10s;
          set $upstream_endpoint https://cache.nixos.org;
        '';

        locations."/" =
        {
          root = "/srv/www/nix-cache-cache";
          index = "index.html";
          extraConfig = ''
            expires max;
            add_header Cache-Control $nix_cache_cache_header always;

            # Ask the upstream server if a file isn't available
            # locally.
            error_page 404 = @fallback;

            # Don't bother logging the above 404.
            log_not_found off;
          '';
        };

        locations."@fallback" = cacheFallbackConfig;

        # We always want to copy cache.nixos.org's nix-cache-info
        # file, and ignore our own, because `nix-push` by default
        # generates one without `Priority` field, and thus that file
        # by default has priority 50 (compared to cache.nixos.org's
        # `Priority: 40`), which will make download clients prefer
        # `cache.nixos.org` over our binary cache.
        locations."= /nix-cache-info" = cacheFallbackConfig;
      };
    };
  };
}
