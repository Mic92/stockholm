{ config, lib, pkgs, ... }:
{
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
      case "$Method $Request_URI" in
        "POST /")
          payload=$(head -c "$req_content_length" \
            | sed 's/+/ /g;s/%\(..\)/\\x\1/g;' \
            | xargs -0 echo -e \
          )
          ${pkgs.curl}/bin/curl -fsSv http://localhost:44001/ \
           -H content-type:application/json \
           -d "$(echo "$payload" | ${pkgs.jq}/bin/jq \
             '{
               command:"PRIVMSG",
               params:["#fysi", "\(.action): \(.comment.html_url // .issue.html_url // .pull_request.html_url)"]
             }'
           )"
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          exit
        ;;
      esac
    ''}'';
  };
}
