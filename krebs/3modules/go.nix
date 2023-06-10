{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.krebs.go;

  out = {
    options.krebs.go = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "Enable go url shortener";
    port = mkOption {
      type = types.int;
      default = 1337;
      description = "on which port go should run on";
    };
  };

  imp = {
    services.redis = {
      enable = true;
    };

    krebs.htgen.go = {
      port = cfg.port;
      script = ''. ${pkgs.writeDash "go" ''
        set -x

        case "$Method $Request_URI" in
          "GET /"*)
            if item=$(${pkgs.redis}/bin/redis-cli --raw get "''${Request_URI#/}"); then
              printf 'HTTP/1.1 302 Found\r\n'
              printf 'Content-Type: text/plain\r\n'
              printf 'Connection: closed\r\n'
              printf 'Location: %s\r\n' "$item"
              printf '\r\n'
              exit
            fi
          ;;
          "POST /")
            uri_candidate=$(head -c "$req_content_length" \
              | sed 's/+/ /g;s/%\(..\)/\\x\1/g;' \
              | xargs -0 echo -e \
            )

            if $(echo "$uri_candidate" | grep -q '^uri=//.*'); then
              # fix urls with missing https: in front
              uri_candidate=$(echo "$uri_candidate" | sed 's,//,https://,g')
            fi

            uri=$(echo "$uri_candidate" | ${pkgs.urix}/bin/urix \
              | head -1 \
            )

            sha256=$(echo "$uri" | sha256sum -b | cut -d\  -f1)
            base32=$(${pkgs.nixStable}/bin/nix-hash --to-base32 --type sha256 "$sha256")
            base32short=$(echo "$base32" | cut -c48-52)
            ${pkgs.redis}/bin/redis-cli set "$base32short" "$uri" >/dev/null

            ref="http://$req_host/$base32short"

            printf 'HTTP/1.1 200 OK\r\n'
            printf 'Content-Type: text/plain; charset=UTF-8\r\n'
            printf 'Connection: close\r\n'
            printf '\r\n'
            printf '%s\n' "$ref"
            exit
          ;;
        esac
      ''}'';
    };
  };

in out
