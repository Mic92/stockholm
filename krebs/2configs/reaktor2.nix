with import <stockholm/lib>;
{ config, pkgs, ... }:

let
  #for shared state directory
  stateDir = config.krebs.reaktor2.r.stateDir;

  generators = pkgs.reaktor2-plugins.generators;
  hooks = pkgs.reaktor2-plugins.hooks;
  commands = pkgs.reaktor2-plugins.commands;

  task = name: let
    rcFile = builtins.toFile "taskrc" ''
      confirmation=no
    '';
  in {
    pattern = "^${name}-([a-z]+)(?::?\\s*(.*))?";
    activate = "match";
    command = 1;
    arguments = [2];
    env.TASKDATA = "${stateDir}/${name}";
    commands = {
      add.filename = pkgs.writeDash "${name}-task-add" ''
        ${pkgs.taskwarrior}/bin/task rc:${rcFile} add "$1"
      '';
      list.filename = pkgs.writeDash "${name}-task-list" ''
        ${pkgs.taskwarrior}/bin/task rc:${rcFile} export \
          | ${pkgs.jq}/bin/jq -r '
              .[] | select(.id != 0) | "\(.id) \(.description)"
            '
      '';
      delete.filename = pkgs.writeDash "${name}-task-delete" ''
        ${pkgs.taskwarrior}/bin/task rc:${rcFile} delete "$1"
      '';
      done.filename = pkgs.writeDash "${name}-task-done" ''
        ${pkgs.taskwarrior}/bin/task rc:${rcFile} done "$1"
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
        (task "agenda")
      ];
    };
  };

in {

  users.users.reaktor2 = {
    uid = genid_uint31 "reaktor2";
    home = stateDir;
  };

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
      username = "reaktor2";
      port = 6697;
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
