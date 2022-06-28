{ config, lib, pkgs, ... }: let

  format-github-message = pkgs.writeDashBin "format-github-message" ''
    set -xefu
    export PATH=${lib.makeBinPath [
      pkgs.jq
    ]}
    INPUT=$(jq -c .)
    if $(echo "$INPUT" | jq 'has("issue") or has("pull_request")'); then
      ${write_to_irc} "$(echo "$INPUT" | jq -r '
        "\(.action): " +
        "[\(.issue.title // .pull_request.title)] " +
        "\(.comment.html_url // .issue.html_url // .pull_request.html_url) "
      ')"
    fi
  '';

  write_to_irc = pkgs.writeDash "write_to_irc" ''
    ${pkgs.curl}/bin/curl -fsSv http://localhost:44001 \
      -H content-type:application/json \
      -d "$(${pkgs.jq}/bin/jq -n \
        --arg text "$1" '{
          command:"PRIVMSG",
          params:["#fysi",$text]
        }'
      )"
  '';

in {
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 44002"; target = "ACCEPT"; }
  ];
  krebs.reaktor2.fysiweb-github = {
    hostname = "irc.libera.chat";
    port = "6697";
    useTLS = true;
    nick = "fysiweb-github";
    API.listen = "inet://127.0.0.1:44001";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#fysi"
          ];
        };
      }
    ];
  };
  krebs.htgen.fysiweb-github = {
    port = 44002;
    user = {
      name = "reaktor2-fysiweb-github";
    };
    script = ''. ${pkgs.writeDash "github-irc" ''
      set -xefu
      case "$Method $Request_URI" in
        "POST /")
          payload=$(head -c "$req_content_length")
          raw=$(printf '%s' "$payload" | ${pkgs.curl}/bin/curl --data-binary @- http://p.krebsco.de | tail -1)
          payload2=$payload
          payload2=$(printf '%s' "$payload" | tr '\n' ' ' | tr -d '\r')
          if [ "$payload" != "$payload2" ]; then
            echo "payload has been mangled" >&2
          else
            echo "payload not mangled" >&2
          fi
          echo "$payload2" | ${format-github-message}/bin/format-github-message
          ${write_to_irc} "$raw"
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          exit
        ;;
      esac
    ''}'';
  };
}
