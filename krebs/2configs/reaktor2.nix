with import <stockholm/lib>;
{ config, pkgs, ... }:

let
  #for shared state directory
  stateDir = config.krebs.reaktor2.r.stateDir;

  generators = pkgs.reaktor2-plugins.generators;
  hooks = pkgs.reaktor2-plugins.hooks;
  commands = pkgs.reaktor2-plugins.commands;

  taskRcFile = builtins.toFile "taskrc" ''
    confirmation=no
  '';

  task = name: {
    pattern = "^${name}-([a-z]+)(?::?\\s*(.*))?";
    activate = "match";
    command = 1;
    arguments = [2];
    env.TASKDATA = "${stateDir}/${name}";
    commands = {
      add.filename = pkgs.writeDash "${name}-task-add" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} add "$1"
      '';
      list.filename = pkgs.writeDash "${name}-task-list" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} export \
          | ${pkgs.jq}/bin/jq -r '
              .[] | select(.id != 0) | "\(.id) \(.description)"
            '
      '';
      delete.filename = pkgs.writeDash "${name}-task-delete" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} delete "$1"
      '';
      done.filename = pkgs.writeDash "${name}-task-done" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} done "$1"
      '';
    };
  };

  systemPlugin = {
    plugin = "system";
    config = {
      workdir = stateDir;
      hooks.JOIN = [
        {
          activate = "always";
          command = {
            filename =
              <stockholm/krebs/5pkgs/simple/Reaktor/scripts/tell-on_join.sh>;
            env = {
              PATH = makeBinPath [
                pkgs.coreutils # XXX env, touch
                pkgs.jq # XXX sed
                pkgs.utillinux # XXX flock
              ];
              state_file = "${stateDir}/tell.json";
            };
          };
        }
      ];
      hooks.PRIVMSG = [
        {
          pattern = "^bier bal(an(ce)?)?$";
          activate = "match";
          command = {
            env = {
              state_file = "${stateDir}/ledger";
            };
            filename = pkgs.writeDash "bier-balance" ''
              ${pkgs.hledger}/bin/hledger -f $state_file bal -N -O csv \
                | ${pkgs.coreutils}/bin/tail +2 \
                | ${pkgs.miller}/bin/mlr --icsv --opprint cat \
                | ${pkgs.gnused}/bin/sed 's/^/the_/'
            '';
          };
        }
        {
          pattern = ''^([\H-]*):?\s+([+-][1-9][0-9]*)\s+(\S+)$'';
          activate = "match";
          arguments = [1 2 3];
          command = {
            env = {
              # TODO; get state as argument
              state_file = "${stateDir}/ledger";
            };
            filename = pkgs.writeDash "ledger-add" ''
              set -x
              tonick=$1
              amt=$2
              unit=$3
              printf '%s\n  %s  %d %s\n  %s  %d %s\n' "$(date -Id)" "$tonick" "$amt" "$unit" "$_from" "$(expr 0 - "''${amt#+}")" "$unit" >> $state_file
            '';
          };
        }
        hooks.sed
        (generators.command_hook {
          inherit (commands) dance random-emoji nixos-version;
          tell = {
            filename =
              <stockholm/krebs/5pkgs/simple/Reaktor/scripts/tell-on_privmsg.sh>;
            env = {
              PATH = makeBinPath [
                pkgs.coreutils # XXX date, env
                pkgs.jq # XXX sed
                pkgs.utillinux # XXX flock
              ];
              state_file = "${stateDir}/tell.txt";
            };
          };
        })
        (task "agenda")
      ];
    };
  };

in {

  users.users.reaktor2 = {
    uid = genid_uint31 "reaktor2";
    home = stateDir;
    isSystemUser = true;
    extraGroups = [ "reaktor2" ];
  };

  users.groups.reaktor2 = {};

  systemd.services.htgen-agenda.serviceConfig.StateDirectory = "reaktor2";
  krebs.htgen.agenda = {
    port = 8009;
    user = {
     name = "reaktor2";
     home = stateDir;
    };
    script = ''. ${pkgs.writeDash "agenda" ''
      echo "$Method $Request_URI" >&2
      case "$Method" in
        "GET")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          TASKDATA=/var/lib/reaktor2/agenda ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} export
          exit
        ;;
      esac
    ''}'';
  };

  services.nginx = {
    virtualHosts."agenda.r" = {
      locations."= /index.html".extraConfig = ''
        alias ${pkgs.writeText "agenda.html" ''
<!DOCTYPE html>
<html>
  <head>
    <title>Agenda</title>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style>
      html {
        font-family: monospace;
      }

      dt {
        float: left;
        clear: left;
        width: 30px;
        text-align: right;
        font-weight: bold;
      }

      dd {
        margin: 0 0 0 40px;
        padding: 0 0 0.5em 0;
      }

      .date {
        color: grey;
        font-style: italic;
      }
    </style>
  </head>
  <body>
    <dl id="agenda"></dl>
    <script>
      const urlSearchParams = new URLSearchParams(window.location.search);
      const params = Object.fromEntries(urlSearchParams.entries());

      if (params.hasOwnProperty("style")) {
        const cssUrls = params["style"].split(" ").filter((x) => x.length > 0);
        for (const cssUrl of cssUrls)
          fetch(cssUrl)
            .then((response) =>
              response.text().then((css) => {
                const title = document.getElementsByTagName("head")[0];
                const style = document.createElement("style");
                style.appendChild(document.createTextNode(css));
                title.appendChild(style);
              })
            )
            .catch(console.log);
      }

      fetch("/agenda.json")
        .then((response) => {
          response.json().then((agenda) => {
            const dl = document.getElementById("agenda");
            for (const agendaItem of agenda) {
              if (agendaItem.status !== "pending") continue;
              // task warrior date format to ISO
              const entryDate = agendaItem.entry.replace(
                /(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z/,
                "$1-$2-$3T$4:$5:$6Z"
              );

              const dt = document.createElement("dt");
              dt.className = "id";
              dt.appendChild(document.createTextNode(agendaItem.id.toString()));
              dl.appendChild(dt);

              const spanDate = document.createElement("span");
              spanDate.className = "date";
              spanDate.title = new Date(entryDate).toString();
              spanDate.appendChild(document.createTextNode(entryDate));

              const link = document.createElement("a");
              link.href = "http://wiki.r/agenda/" + encodeURIComponent(agendaItem.description.replaceAll("/", "\u29F8")); // we use big solidus instead of slash because gollum will create directories
              link.appendChild(document.createTextNode(agendaItem.description));

              const dd = document.createElement("dd");
              dd.className = "description";
              dd.appendChild(link);
              dd.appendChild(document.createTextNode(" "));
              dd.appendChild(spanDate);

              dl.appendChild(dd);
            }
          });
        })
        .then((data) => console.log(data));
    </script>
  </body>
</html>
        ''};
      '';
      locations."/agenda.json".extraConfig = ''
        proxy_set_header Host $host;
        proxy_pass http://localhost:8009;
      '';
      extraConfig = ''
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
      '';
    };
  };

  systemd.services.reaktor2-r.serviceConfig.DynamicUser = mkForce false;
  systemd.services.reaktor2-hackint.serviceConfig.DynamicUser = mkForce false;
  krebs.reaktor2 = {
    hackint = {
      hostname = "irc.hackint.org";
      nick = "reaktor2|krebs";
      plugins = [
        {
          plugin = "register";
          config = {
            channels = [
              "#krebs"
            ];
          };
        }
        systemPlugin
      ];
      username = "reaktor2";
      port = "6697";
    };
    r = {
      nick = "reaktor2|krebs";
      sendDelaySec = null;
      plugins = [
        {
          plugin = "register";
          config = {
            channels = [
              "#noise"
              "#xxx"
            ];
          };
        }
        systemPlugin
      ];
      username = "reaktor2";
    };
  };
}
