with import <stockholm/lib>;
{ config, pkgs, ... }:

let
  #for shared state directory
  stateDir = config.krebs.reaktor2.r.stateDir;

  generators = pkgs.reaktor2-plugins.generators;
  hooks = pkgs.reaktor2-plugins.hooks;
  commands = pkgs.reaktor2-plugins.commands;

  # bedger - the bier ledger
  #
  # logo: http://c.r/bedger2
  #
  bedger-add = {
    pattern = ''^([\H-]*?):?\s+([+-][1-9][0-9]*)\s+(\S+)$'';
    activate = "match";
    arguments = [1 2 3];
    command = {
      env = {
        # TODO; get state as argument
        state_file = "${stateDir}/ledger";
      };
      filename = pkgs.writeDash "bedger-add" ''
        set -x
        tonick=$1
        amt=$2
        unit=$3
        printf '%s\n  %s  %d %s\n  %s  %d %s\n' "$(date -Id)" "$tonick" "$amt" "$unit" "$_from" "$(expr 0 - "''${amt#+}")" "$unit" >> $state_file
        ${pkgs.hledger}/bin/hledger -f $state_file bal -N -O csv \
          | ${pkgs.coreutils}/bin/tail +2 \
          | ${pkgs.miller}/bin/mlr --icsv --opprint cat \
          | ${pkgs.gnugrep}/bin/grep "$_from"
      '';
    };
  };
  bedger-balance = {
    pattern = "^bier (ballern|bal(an(ce)?)?)$";
    activate = "match";
    command = {
      env = {
        state_file = "${stateDir}/ledger";
      };
      filename = pkgs.writeDash "bedger-balance" ''
        ${pkgs.hledger}/bin/hledger -f $state_file bal -N -O csv \
          | ${pkgs.coreutils}/bin/tail +2 \
          | ${pkgs.miller}/bin/mlr --icsv --opprint cat \
          | ${pkgs.gnused}/bin/sed 's/^\(.\)/\1‍/'
      '';
    };
  };

  bing = {
    pattern = "!bing (.*)$";
    activate = "match";
    arguments = [1];
    timeoutSec = 1337;
    command = {
      filename = pkgs.writeDash "bing" ''
        set -efu
        report_error() {
          printf '%s' "$*" |
            curl -Ss http://p.r --data-binary @- |
            tail -1 |
            echo "error $(cat)"
          exit 0
        }
        export PATH=${makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ]}
        response=$(printf '%s' "$*" |
          curl -SsG http://bing-gpt.r/api/chat --data-urlencode 'prompt@-'
        )
        if [ "$?" -ne 0 ]; then
          report_error "$response"
        else
          if ! text=$(printf '%s' "$response" | jq -er '.item.messages[1].text'); then
            echo "$_from: $(report_error "$response")"
            exit 0
          fi
          printf '%s' "$text" | echo "$_from: $(cat)"

          printf '%s' "$response" |
            jq -r '[.item.messages[1].sourceAttributions[].seeMoreUrl] | to_entries[] | "[\(.key + 1)]: \(.value)"'
        fi
      '';
    };
  };

  confuse = {
    pattern = "!confuse (.*)$";
    activate = "match";
    arguments = [1];
    command = {
      filename = pkgs.writeDash "confuse" ''
        set -efux

        export PATH=${makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.stable-generate
        ]}
        paste_url=$(stable-generate "$@" |
          curl -Ss http://p.r --data-binary @- |
          tail -1
        )
        echo "$_from: $paste_url"
      '';
    };
  };
  interrogate = {
    pattern = "^!interrogate (.*)$";
    activate = "match";
    arguments = [1];
    command = {
      filename = pkgs.writeDash "interrogate" ''
        set -efux

        export PATH=${makeBinPath [
          pkgs.stable-interrogate
        ]}
        caption=$(stable-interrogate "$@")
        echo "$_from: $caption"
      '';
    };
  };

  confuse_hackint = {
    pattern = "!confuse (.*)$";
    activate = "match";
    arguments = [1];
    command = {
      filename = pkgs.writeDash "confuse" ''
        set -efu
        export PATH=${makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.stable-generate
        ]}
        case $_msgtarget in \#*)
          paste_url=$(stable-generate "$@" |
            curl -Ss https://p.krebsco.de --data-binary @- |
            tail -1
          )
          echo "$_from: $paste_url"
        esac
      '';
    };
  };

  say = {
    pattern = "^!say (.*)$";
    activate = "match";
    arguments = [1];
    command = {
      filename = pkgs.writeDash "say" ''
        set -efu

        export PATH=${makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.opusTools
        ]}
        paste_url=$(printf '%s' "$1" |
          curl -fSsG http://tts.r/api/tts --data-urlencode 'text@-' |
          opusenc - - |
          curl -Ss https://p.krebsco.de --data-binary @- |
          tail -1
        )
        echo "$_from: $paste_url"
      '';
    };
  };

  taskRcFile = builtins.toFile "taskrc" ''
    confirmation=no
  '';

  task = name: {
    pattern = "^${name}-([a-z]+)(?::?\\s*(.*))?";
    activate = "match";
    command = 1;
    arguments = [2];
    env.TASKDATA = "${stateDir}/${name}";
    commands = rec {
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
      del = delete;
      done.filename = pkgs.writeDash "${name}-task-done" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} done "$1"
      '';
    };
  };

  locationsLib = pkgs.writeText "locations.sh" ''
    ENDPOINT=http://c.r/poi.json
    get_locations() {
      curl -fsS "$ENDPOINT"
    }

    set_locations() {
      curl -fSs --data-binary @- "$ENDPOINT"
    }

    set_location() {
      [ $# -eq 3 ] || return 1
      get_locations \
        | jq \
            --arg name "$1" \
            --arg latitude "$2" \
            --arg longitude "$3" \
            '.[$name] = { $latitude, $longitude }' \
        | set_locations
    }

    get_location() {
      [ $# -eq 1 ] || return 1
      get_locations | jq --arg name "$1" '.[$name]'
    }

    delete_location() {
      [ $# -eq 1 ] || return 1
      get_locations | jq --arg name "$1" 'del(.[$name])' | set_locations
    }
  '';

  systemPlugin = { extra_privmsg_hooks ? [] }: {
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
                pkgs.util-linux # XXX flock
              ];
              state_file = "${stateDir}/tell.json";
            };
          };
        }
      ];
      hooks.PRIVMSG = [
        {
          pattern = "^list-locations";
          activate = "match";
          command = {
            filename = pkgs.writeDash "list-locations" ''
              export PATH=${makeBinPath [
                pkgs.curl
                pkgs.jq
              ]}
              set -efu
              set -x
              . ${locationsLib}
              get_locations | jq -r 'to_entries[]|"\(.key) \(.value.latitude),\(.value.longitude)"'
            '';
          };
        }
        {
          pattern = ''^add-location (\S+) ([0-9.]+),([0-9.]+)$'';
          activate = "match";
          arguments = [1 2 3];
          command = {
            filename = pkgs.writeDash "add-location" ''
              export PATH=${makeBinPath [
                pkgs.curl
                pkgs.jq
              ]}
              set -efu
              set -x
              . ${locationsLib}
              set_location "$1" $2 $3
            '';
          };
        }
        {
          pattern = ''^delete-location (\S+)$'';
          activate = "match";
          arguments = [1];
          command = {
            filename = pkgs.writeDash "add-location" ''
              export PATH=${makeBinPath [
                pkgs.curl
                pkgs.jq
              ]}
              set -efu
              set -x
              . ${locationsLib}
              delete_location "$1"
            '';
          };
        }
        {
          pattern = ''^18@p\s+(\S+)\s+(\d+)m$'';
          activate = "match";
          arguments = [1 2];
          command = {
            env = {
              CACHE_DIR = "${stateDir}/krebsfood";
            };
            filename =
            let
              osm-restaurants-src = pkgs.fetchFromGitHub {
                owner = "kmein";
                repo = "scripts";
                rev = "dda381be26abff73a0cf364c6dfff6e1701f41ee";
                sha256 = "sha256-J7jGWZeAULDA1EkO50qx+hjl+5IsUj389pUUMreKeNE=";
              };
              osm-restaurants = pkgs.callPackage "${osm-restaurants-src}/osm-restaurants" {};
            in pkgs.writeDash "krebsfood" ''
              set -efu
              export PATH=${makeBinPath [
                osm-restaurants
                pkgs.coreutils
                pkgs.curl
                pkgs.jq
              ]}
              poi=$(curl -fsS http://c.r/poi.json | jq --arg name "$1" '.[$name]')
              if [ "$poi" = null ]; then
                latitude=52.51252
                longitude=13.41740
              else
                latitude=$(echo "$poi" | jq -r .latitude)
                longitude=$(echo "$poi" | jq -r .longitude)
              fi

              for api_endpoint in \
                https://lz4.overpass-api.de/api/interpreter \
                https://z.overpass-api.de/api/interpreter \
                https://maps.mail.ru/osm/tools/overpass/api/interpreter \
                https://overpass.openstreetmap.ru/api/interpreter \
                https://overpass.kumi.systems/api/interpreter
              do
                restaurant=$(osm-restaurants --endpoint "$api_endpoint" --radius "$2" --latitude "$latitude" --longitude "$longitude")
                if [ "$?" -eq 0 ]; then
                  break
                fi
              done
              printf '%s' "$restaurant" | tail -1 | jq -r '"How about \(.tags.name) (https://www.openstreetmap.org/\(.type)/\(.id)), open \(.tags.opening_hours)?"'
            '';
          };
        }
        bedger-add
        bedger-balance
        bing
        hooks.sed
        interrogate
        say
        (generators.command_hook {
          inherit (commands) dance random-emoji nixos-version;
          tell = {
            filename =
              <stockholm/krebs/5pkgs/simple/Reaktor/scripts/tell-on_privmsg.sh>;
            env = {
              PATH = makeBinPath [
                pkgs.coreutils # XXX date, env
                pkgs.jq # XXX sed
                pkgs.util-linux # XXX flock
              ];
              state_file = "${stateDir}/tell.txt";
            };
          };
        })
        (task "agenda")
      ] ++ extra_privmsg_hooks;
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
      serverAliases = [ "kri.r" ];
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
        (systemPlugin {
          extra_privmsg_hooks = [
            confuse_hackint
          ];
        })
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
        (systemPlugin {
          extra_privmsg_hooks = [
            confuse
          ];
        })
      ];
      username = "reaktor2";
    };
  };
}
