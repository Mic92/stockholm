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
        "\(.comment.html_url // .issue.html_url // .pull_request.html_url) " +
        "by \(.comment.user.login // .issue.user.login // .pull_request.user.login)"
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
          echo "$payload" >&2
          payload2=$payload
          payload2=$(echo "$payload" | tr '\n' ' ' | tr -d '\r')
          if [ "$payload" != "$payload2" ]; then
            echo "payload has been mangled" >&2
          else
            echo "payload not mangled" >&2
          fi
          echo "$payload2" > /tmp/last_fysi_payload
          echo "$payload2" | ${format-github-message}/bin/format-github-message
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          exit
        ;;
      esac
    ''}'';
  };
}
