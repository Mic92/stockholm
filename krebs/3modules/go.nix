{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

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
    krebs.htgen.go = {
      port = cfg.port;
      script = ''. ${pkgs.writeDash "go" ''
        find_item() {
          if test ''${#1} -ge 7; then
            set -- "$(find "$STATEDIR/items" -mindepth 1 -maxdepth 1 \
                -regex "$STATEDIR/items/$1[0-9A-Za-z]*$")"
            if test -n "$1" && test $(echo "$1" | wc -l) = 1; then
              echo "$1"
              return 0
            fi
          fi
          return 1
        }

        STATEDIR=$HOME
        mkdir -p "$STATEDIR/items"

        case "$Method $Request_URI" in
          "GET /"*)
            if item=$(find_item "''${Request_URI#/}"); then
              uri=$(cat "$item")
              printf 'HTTP/1.1 302 Found\r\n'
              printf 'Content-Type: text/plain\r\n'
              printf 'Connection: closed\r\n'
              printf 'Location: %s\r\n' "$uri"
              printf '\r\n'
              exit
            fi
          ;;
          "POST /")
            uri=$(mktemp -t htgen.$$.content.XXXXXXXX)
            trap 'rm $uri >&2' EXIT

            head -c "$req_content_length" \
              | sed 's/+/ /g;s/%\(..\)/\\x\1/g;' \
              | xargs -0 echo -e \
              | tee /tmp/tee.log \
              | ${pkgs.urix}/bin/urix \
              | head -1 \
              > "$uri"
            sha256=$(sha256sum -b "$uri" | cut -d\  -f1)
            base32=$(${pkgs.nixStable}/bin/nix-hash --to-base32 --type sha256 "$sha256")
            item="$STATEDIR/items/$base32"
            ref="http://$req_host/$base32"

            if ! test -e "$item"; then
              mkdir -v -p "$STATEDIR/items" >&2
              cp -v "$uri" "$item" >&2
            fi

            base32short=$(echo "$base32" | cut -b-7)
            if item=$(find_item "$base32short"); then
              ref="http://$req_host/$base32short"
            fi

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
