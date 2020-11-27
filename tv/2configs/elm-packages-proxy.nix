{ config, lib, pkgs, ... }: let

  cfg.nameserver = "1.1.1.1";
  cfg.packageDir = "/var/lib/elm-packages";
  cfg.port = 7782;

  # TODO secret files
  cfg.htpasswd = "/var/lib/certs/package.elm-lang.org/htpasswd";
  cfg.sslCertificate = "/var/lib/certs/package.elm-lang.org/fullchain.pem";
  cfg.sslCertificateKey = "/var/lib/certs/package.elm-lang.org/key.pem";

  semverRegex =
    "(?<major>0|[1-9]\\d*)\\.(?<minor>0|[1-9]\\d*)\\.(?<patch>0|[1-9]\\d*)(?:-(?<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+(?<buildmetadata>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?";

in {
  services.nginx.virtualHosts."package.elm-lang.org" = {
    addSSL = true;

    sslCertificate = cfg.sslCertificate;
    sslCertificateKey = cfg.sslCertificateKey;

    locations."/all-packages".extraConfig = ''
      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.elm-packages-proxy.port};
      proxy_pass_header Server;
    '';

    locations."/all-packages/since/".extraConfig = ''
      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.elm-packages-proxy.port};
      proxy_pass_header Server;
    '';

    locations."~ ^/packages/(?<author>[A-Za-z0-9-]+)/(?<pname>[A-Za-z0-9-]+)/(?<version>${semverRegex})\$".extraConfig = ''
      auth_basic "Restricted Area";
      auth_basic_user_file ${cfg.htpasswd};

      proxy_set_header X-User $remote_user;
      proxy_set_header X-Author $author;
      proxy_set_header X-Package $pname;
      proxy_set_header X-Version $version;
      proxy_pass_header Server;

      if ($request_method != POST) {
        return 405;
      }

      proxy_pass http://127.0.0.1:${toString config.krebs.htgen.elm-packages-proxy.port};
    '';

    locations."~ ^/packages/(?<author>[A-Za-z0-9-]+)/(?<pname>[A-Za-z0-9-]+)/(?<version>${semverRegex})/(?:zipball|elm.json|endpoint.json)\$".extraConfig = ''
      set $zipball "${cfg.packageDir}/$author/$pname/$version/zipball";
      proxy_set_header X-Author $author;
      proxy_set_header X-Package $pname;
      proxy_set_header X-Version $version;
      proxy_set_header X-Zipball $zipball;
      proxy_pass_header Server;
      resolver ${cfg.nameserver};

      if (-f $zipball) {
        set $new_uri http://127.0.0.1:${toString config.krebs.htgen.elm-packages-proxy.port};
      }
      if (!-f $zipball) {
        set $new_uri https://package.elm-lang.org$request_uri;
      }

      proxy_pass $new_uri;
    '';
  };

  krebs.htgen.elm-packages-proxy = {
    port = cfg.port;
    script = /* sh */ ''. ${pkgs.writeDash "elm-packages-proxy.sh" ''
      PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.curl
        pkgs.findutils
        pkgs.gnugrep
        pkgs.jq
        pkgs.unzip
      ]}
      export PATH
      file_response() {(
        status_code=$1
        status_reason=$2
        file=$3
        content_type=$4

        content_length=$(wc -c "$file" | cut -d\  -f1)

        printf "HTTP/1.1 $status_code $status_reason\r\n"
        printf 'Connection: close\r\n'
        printf 'Content-Length: %d\r\n' "$content_length"
        printf 'Content-Type: %s\r\n' "$content_type"
        printf 'Server: %s\r\n' "$Server"
        printf '\r\n'
        cat "$file"
      )}
      string_response() {(
        status_code=$1
        status_reason=$2
        response_body=$3
        content_type=$4

        printf "HTTP/1.1 $status_code $status_reason\r\n"
        printf 'Connection: close\r\n'
        printf 'Content-Length: %d\r\n' ''${#response_body}
        printf 'Content-Type: %s\r\n' "$content_type"
        printf 'Server: %s\r\n' "$Server"
        printf '\r\n'
        printf '%s\n' "$response_body"
      )}

      case "$Method $Request_URI" in
        'GET /packages/'*)

          author=$req_x_author
          pname=$req_x_package
          version=$req_x_version

          zipball=${cfg.packageDir}/$author/$pname/$version/zipball
          elmjson=$HOME/cache/$author%2F$pname%2F$version%2Felm.json
          endpointjson=$HOME/cache/$author%2F$pname%2F$version%2Fendpoint.json
          mkdir -p "$HOME/cache"

          case $(basename $Request_URI) in
            zipball)
              file_response 200 OK "$zipball" application/zip
              exit
            ;;
            elm.json)
              if ! test -f "$elmjson"; then
                unzip -p "$zipball" \*/elm.json > "$elmjson"
              fi
              file_response 200 OK "$elmjson" 'application/json; charset=UTF-8'
              exit
            ;;
            endpoint.json)
              if ! test -f "$endpointjson"; then
                hash=$(sha1sum "$zipball" | cut -d\  -f1)
                url=https://package.elm-lang.org/packages/$author/$pname/$version/zipball
                jq -n \
                    --arg hash "$hash" \
                    --arg url "$url" \
                    '{ $hash, $url }' \
                  > "$endpointjson"
              fi
              file_response 200 OK "$endpointjson" 'application/json; charset=UTF-8'
              exit
            ;;
          esac
        ;;
        'POST /packages/'*)

          author=$req_x_author
          pname=$req_x_package
          user=$req_x_user
          version=$req_x_version

          zipball=${cfg.packageDir}/$author/$pname/$version/zipball

          if test -e "$zipball"; then
            string_response 409 Conflict \
                "package already exists: $author/$pname@$version" \
                text/plain
          else
            echo "user $user is uploading package $pname@$version" >&2
            mkdir -p "$(dirname "$zipball")"
            head -c $req_content_length > "$zipball"
            string_response 200 OK \
                "package created: $author/$pname@$version" \
                text/plain
          fi
          exit
        ;;
        'POST /all-packages')

          response=$(mktemp -t htgen.$$.elm-packages-proxy.all-packages.XXXXXXXX)
          trap "rm $response >&2" EXIT

          {
            # upstream packages
            curl -fsS https://package.elm-lang.org"$Request_URI"

            # private packages
            (cd ${cfg.packageDir}; find -mindepth 3 -maxdepth 3) |
            jq -Rs '
              split("\n") |
              map(
                select(.!="") |
                match("^\\./(?<author>[^/]+)/(?<pname>[^/]+)/(?<version>[^/]+)$").captures |
                map({key:.name,value:.string}) |
                from_entries
              ) |
              reduce .[] as $item ({};
                ($item|"\(.author)/\(.pname)") as $name |
                . + { "\($name)": ((.[$name] // []) + [$item.version]) }
              )
            '
          } |
          jq -cs add > $response

          file_response 200 OK "$response" 'application/json; charset=UTF-8'
          exit
        ;;
        'POST /all-packages/since/'*)

          response=$(mktemp -t htgen.$$.elm-packages-proxy.all-packages.XXXXXXXX)
          trap "rm $response >&2" EXIT

          {
            # upstream packages
            curl -fsS https://package.elm-lang.org"$Request_URI"

            # private packages
            (cd ${cfg.packageDir}; find -mindepth 3 -maxdepth 3) |
            jq -Rs '
              split("\n") |
              map(
                select(.!="") |
                sub("^\\./(?<author>[^/]+)/(?<pname>[^/]+)/(?<version>[^/]+)$";"\(.author)/\(.pname)@\(.version)")
              ) |
              sort_by(split("@") | [.[0]]+(.[1]|split("."))) |
              reverse
            '
          } |
          jq -cs add > $response

          file_response 200 OK "$response" 'application/json; charset=UTF-8'
          exit
        ;;
      esac
    ''}'';
  };
}
