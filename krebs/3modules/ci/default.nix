{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.krebs.ci;

  out = {
    options.krebs.ci = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "Enable krebs ci service";
    repos = mkOption {
      type = types.attrsOf (types.submodule ({ config, ...}: {
        options = {
          urls = mkOption {
            type = types.listOf types.str;
            default = [ "git@localhost:${config._module.args.name}" ];
          };
        };
      }));
    };
  };

  hostname = config.networking.hostName;
  getJobs = pkgs.writeDash "get_jobs" ''
    set -efu
    ${pkgs.nix}/bin/nix-build --no-out-link --quiet --show-trace -Q ./ci.nix >&2
    json="$(${pkgs.nix}/bin/nix-instantiate --quiet -Q --eval --strict --json ./ci.nix)"
    echo "$json" | ${pkgs.jq}/bin/jq -r 'to_entries[] | [.key, .value] | @tsv' \
      | while read -r host builder; do
        gcroot=${shell.escape profileRoot}/$host-builder
        ${pkgs.nix}/bin/nix-env -p "$gcroot" --set "$builder"
      done
    echo "$json"
  '';

  profileRoot = "/nix/var/nix/profiles/ci";

  bcfg = config.services.buildbot-master;

  imp = {
    services.buildbot-master = {
      workers = [ "worker.Worker('testworker', 'pass')" ];

      changeSource = mapAttrsToList (name: repo:
        concatMapStringsSep "," (url: ''
          changes.GitPoller(
              "${url}",
              workdir='${name}-${elemAt(splitString "." url) 1}', branches=True,
              project='${name}',
              pollinterval=30
          )
        '') repo.urls
      ) cfg.repos;

      schedulers = mapAttrsToList (name: repo: ''
        schedulers.SingleBranchScheduler(
            change_filter=util.ChangeFilter(
                branch_re=".*",
                project='${name}',
            ),
            treeStableTimer=60,
            name="${name}-all-branches",
            builderNames=[
                "${name}",
            ]
        ),
        schedulers.ForceScheduler(
            name="${name}",
            builderNames=[
                "${name}",
            ]
        )
      '') cfg.repos;

      builders = [];

      extraConfig = ''
        # https://docs.buildbot.net/latest/manual/configuration/buildfactories.html
        from buildbot.plugins import util, steps
        from buildbot.process import buildstep, logobserver
        from twisted.internet import defer
        import json
        import sys

        class GenerateStagesCommand(buildstep.ShellMixin, steps.BuildStep):
            def __init__(self, **kwargs):
                kwargs = self.setupShellMixin(kwargs)
                super().__init__(**kwargs)
                self.observer = logobserver.BufferLogObserver()
                self.addLogObserver('stdio', self.observer)

            def extract_stages(self, stdout):
                stages = json.loads(stdout)
                return stages

            @defer.inlineCallbacks
            def run(self):
                # run nix-instanstiate to generate the dict of stages
                cmd = yield self.makeRemoteShellCommand()
                yield self.runCommand(cmd)

                # if the command passes extract the list of stages
                result = cmd.results()
                if result == util.SUCCESS:
                    # create a ShellCommand for each stage and add them to the build
                    stages = self.extract_stages(self.observer.getStdout())
                    self.build.addStepsAfterCurrentStep([
                        steps.ShellCommand(
                          name=stage,
                          env=dict(
                            build_name = stage,
                            build_script = stages[stage],
                          ),
                          timeout = 3600,
                          command="${pkgs.writeDash "build.sh" ''
                            set -xefu
                            profile=${shell.escape profileRoot}/$build_name
                            result=$("$build_script")
                            if [ -n "$result" ]; then
                              ${pkgs.nix}/bin/nix-env -p "$profile" --set "$result"
                            fi
                          ''}",
                        ) for stage in stages
                    ])

                return result


        ${concatStringsSep "\n" (mapAttrsToList (name: repo: ''
          factory_${name} = util.BuildFactory()
          factory_${name}.addStep(steps.Git(
              repourl=util.Property('repository', '${head repo.urls}'),
              method='clobber',
              mode='full',
              submodules=True,
          ))

          factory_${name}.addStep(GenerateStagesCommand(
              env={
                  "NIX_REMOTE": "daemon",
                  "NIX_PATH": "secrets=/var/src/stockholm/null:/var/src",
              },
              name="Generate build stages",
              command=[
                  "${getJobs}"
              ],
              haltOnFailure=True,
          ))

          c['builders'].append(
              util.BuilderConfig(
                  name="${name}",
                  workernames=['testworker'],
                  factory=factory_${name}
              )
          )
        '') cfg.repos)}

        # fancy irc notification by Mic92 https://github.com/Mic92/dotfiles/tree/master/nixos/eve/modules/buildbot
        sys.path.append("${./modules}")
        from irc_notify import NotifyFailedBuilds
        c['services'].append(
            NotifyFailedBuilds("irc://buildbot|test@irc.r:6667/#xxx")
        )

      '';

      enable = true;
      reporters = [
        ''
          reporters.IRC(
            host = "irc.r",
            nick = "buildbot|${hostname}",
            notify_events = [ 'started', 'finished', 'failure', 'success', 'exception', 'problem' ],
            channels = [{"channel": "#xxx"}],
            showBlameList = True,
            authz={'force': True},
          )
        ''
      ];

      buildbotUrl = "http://build.${hostname}.r/";
    };

    services.buildbot-worker = {
      enable = true;
      workerUser = "testworker";
      workerPass = "pass";
      packages = with pkgs; [ git gnutar gzip jq nix populate ];
    };

    system.activationScripts.buildbots-nix-profile = ''
      ${pkgs.coreutils}/bin/mkdir -p ${shell.escape profileRoot}
      ${pkgs.coreutils}/bin/chmod 0770 ${shell.escape profileRoot}
      ${pkgs.coreutils}/bin/chgrp buildbots ${shell.escape profileRoot}
    '';

    users = {
      groups.buildbots.gid = genid "buildbots";
      users = {
        buildbot.extraGroups = [ "buildbots" ];
        bbworker.extraGroups = [ "buildbots" ];
      };
    };
  };

in out
