{ config, lib, pkgs, ... }:
with import ../../lib/pure.nix { inherit lib; };

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
      filename = pkgs.writers.writeDash "bedger-add" ''
        set -x
        tonick=$1
        amt=$2
        unit=$3
        printf '%s\n  %s  %d %s\n  %s  %d %s\n' "$(date -Id)" "$tonick" "$amt" "$unit" "$_from" "$(expr 0 - "''${amt#+}")" "$unit" >> $state_file
        ${pkgs.hledger}/bin/hledger -f "$state_file" bal -N -O csv \
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
      filename = pkgs.writers.writeDash "bedger-balance" ''
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
      filename = pkgs.writers.writeDash "bing" ''
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
          if ! text=$(printf '%s' "$response" | jq -er '.item.messages[-1].text'); then
            echo "$_from: $(report_error "$response")"
            exit 0
          fi
          # value seems to be 512 - overhead
          echo "$_from: $text" | fold -s -w 426

          printf '%s' "$response" |
            jq -r '[.item.messages[-1].sourceAttributions[].seeMoreUrl] | to_entries[] | "[\(.key + 1)]: \(.value)"'
        fi
      '';
    };
  };

  bing-img = {
    pattern = "!bing-img (.*)$";
    activate = "match";
    arguments = [1];
    timeoutSec = 1337;
    command = {
      filename = pkgs.writers.writeDash "bing-img" ''
        set -efu
        report_error() {
          printf '%s' "$*" |
            curl -Ss http://p.r --data-binary @- |
            tail -1 |
            echo "error $(cat)"
          exit 0
        }
        export PATH=${makeBinPath [
          pkgs.dash
          pkgs.coreutils
          pkgs.curl
          pkgs.findutils
          pkgs.jq
        ]}
        response=$(printf '%s' "$*" |
          curl -SsG http://bing-gpt.r/api/images --data-urlencode 'prompt@-'
        )
        if [ "$?" -ne 0 ]; then
          report_error "$response"
        else
          if ! text=$(
            printf '%s' "$response" |
              jq -er '.[].url'
          ); then
            echo "$_from: $(report_error "$response")"
            exit 0
          fi
          echo "$text" |
            xargs -I {} dash -c 'curl -Ss {} |
              curl -Ss https://p.krebsco.de --data-binary @- |
              tail -1' |
            tr '\n' ' ' |
            echo "$_from: $(cat)"
        fi
      '';
    };
  };

  confuse = {
    pattern = "!confuse (.*)$";
    activate = "match";
    arguments = [1];
    command = {
      filename = pkgs.writers.writeDash "confuse" ''
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
      filename = pkgs.writers.writeDash "interrogate" ''
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
      filename = pkgs.writers.writeDash "confuse" ''
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
      filename = pkgs.writers.writeDash "say" ''
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
      add.filename = pkgs.writers.writeDash "${name}-task-add" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} add "$1"
      '';
      list.filename = pkgs.writers.writeDash "${name}-task-list" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} export \
          | ${pkgs.jq}/bin/jq -r '
              .[] | select(.id != 0) | "\(.id) \(.description)"
            '
      '';
      delete.filename = pkgs.writers.writeDash "${name}-task-delete" ''
        ${pkgs.taskwarrior}/bin/task rc:${taskRcFile} delete "$1"
      '';
      del = delete;
      done.filename = pkgs.writers.writeDash "${name}-task-done" ''
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
            filename = ../5pkgs/simple/Reaktor/scripts/tell-on_join.sh;
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
            filename = pkgs.writers.writeDash "list-locations" ''
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
            filename = pkgs.writers.writeDash "add-location" ''
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
            filename = pkgs.writers.writeDash "add-location" ''
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
            in pkgs.writers.writeDash "krebsfood" ''
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
        bing-img
        hooks.sed
        interrogate
        say
        (generators.command_hook {
          inherit (commands) dance random-emoji nixos-version;
          tell = {
            filename = ../5pkgs/simple/Reaktor/scripts/tell-on_privmsg.sh;
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
    script = ''. ${pkgs.writers.writeDash "agenda" ''
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

  services.nginx.virtualHosts."agenda.r" = {
    serverAliases = [ "kri.r" ];
    locations."= /index.html".extraConfig = ''
      alias ${./agenda.html};
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

  krebs.htgen.bedger = {
    port = 8011;
    user = {
     name = "reaktor2";
     home = stateDir;
    };
    script = ''. ${pkgs.writers.writeDash "bedger" ''
      case "$Method" in
        "GET")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          ${pkgs.hledger}/bin/hledger -f ${stateDir}/ledger bal -N -O json
          exit
        ;;
      esac
    ''}'';
  };

  services.nginx.virtualHosts."hotdog.r" = {
    locations."/bedger.json".extraConfig = ''
      proxy_set_header Host $host;
      proxy_pass http://localhost:8011;
    '';
    extraConfig = ''
      add_header 'Access-Control-Allow-Origin' '*';
      add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
    '';
    # needed for acmeFallback in sync-containers, or other machines not reachable globally
    locations."~ ^/.well-known/acme-challenge/".root = "/var/lib/acme/acme-challenge";
  };

  services.nginx.virtualHosts."bedge.r" = {
    locations."/".extraConfig = ''
      proxy_set_header Host $host;
      proxy_pass http://localhost:${toString config.services.hledger-web.port};
    '';
    locations."/bedger.json".extraConfig = ''
      proxy_set_header Host $host;
      proxy_pass http://localhost:8011;
    '';
    extraConfig = ''
      add_header 'Access-Control-Allow-Origin' '*';
      add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
    '';
  };
  services.hledger-web = {
    enable = true;
  };

  systemd.services.reaktor2-r.serviceConfig.DynamicUser = mkForce false;
  systemd.services.reaktor2-hackint.serviceConfig.DynamicUser = mkForce false;
  krebs.reaktor2 = {
    hackint = {
      hostname = "irc.hackint.org";
      nick = "reaktor";
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
      nick = "reaktor";
      sendDelaySec = null;
      plugins = [
        {
          plugin = "register";
          config = {
            channels = [
              "#noise"
              "#xxx"
              "#fin"
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
