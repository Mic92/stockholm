with import <stockholm/lib>;
{ config, pkgs, ... }:

let
  stateDir = config.krebs.reaktor2.r.stateDir;

  generators = pkgs.reaktor2-plugins.generators;
  hooks = pkgs.reaktor2-plugins.hooks;
  commands = pkgs.reaktor2-plugins.commands;

  task = name: let
    rcFile = builtins.toFile "taskrc" ''
      confirmation=no
    '';
  in {
    "${name}-task-add" = {
      pattern = "^${name}-add: (.*)$";
      activate = "match";
      arguments = [1];
      command = {
        env = {
          TASKDATA = "${stateDir}/${name}";
        };
        filename = pkgs.writeDash "${name}-task-add" ''
          ${pkgs.taskwarrior}/bin/task rc:${rcFile} add "$*"
        '';
      };
    };

    "${name}-task-list" = {
      pattern = "^${name}-list$";
      activate = "match";
      command = {
        env = {
          TASKDATA = "${stateDir}/${name}";
        };
        filename = pkgs.writeDash "${name}-task-list" ''
          ${pkgs.taskwarrior}/bin/task rc:${rcFile} export | ${pkgs.jq}/bin/jq -r '.[] | select(.id != 0) | "\(.id) \(.description)"'
        '';
      };
    };

    "${name}-task-delete" = {
      pattern = "^${name}-delete: (.*)$";
      activate = "match";
      arguments = [1];
      command = {
        env = {
          TASKDATA = "${stateDir}/${name}";
        };
        filename = pkgs.writeDash "${name}-task-delete" ''
          ${pkgs.taskwarrior}/bin/task rc:${rcFile} delete "$*"
        '';
      };
    };

    "${name}-task-done" = {
      pattern = "^${name}-done: (.*)$";
      activate = "match";
      arguments = [1];
      command = {
        env = {
          TASKDATA = "${stateDir}/${name}";
        };
        filename = pkgs.writeDash "${name}-task-done" ''
          ${pkgs.taskwarrior}/bin/task rc:${rcFile} done "$*"
        '';
      };
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
              "${pkgs.Reaktor.src}/reaktor/commands/tell-on_join";
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
          pattern = "^ledger balance";
          activate = "match";
          command = {
            env = {
              state_file = "${stateDir}/ledger";
            };
            filename = pkgs.writeDash "ledger-balance" ''
              ${pkgs.hledger}/bin/hledger -f $state_file bal -N
            '';
          };
        }
        {
          pattern = ''^(\S+)\s+([+-][1-9][0-9]*)\s+(\S+)$'';
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
          inherit (commands) hello random-emoji nixos-version stockholm-issue;
          tell = {
            filename =
              "${pkgs.Reaktor.src}/reaktor/commands/tell-on_privmsg";
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
      ] ++ (attrValues (task "agenda"))
      ;
    };
  };

in {

  krebs.reaktor2 = {
    freenode = {
      hostname = "irc.freenode.org";
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
      user = "reaktor2";
    };
    r = {
      nick = "reaktor2|krebs";
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
      user = "reaktor2";
    };
  };
}
