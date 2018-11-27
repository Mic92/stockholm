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
    ${pkgs.nix}/bin/nix-build --no-out-link --quiet -Q ./ci.nix > /dev/null
    js="$(${pkgs.nix}/bin/nix-instantiate --quiet -Q --eval --strict --json ./ci.nix)"
    echo "$js" | ${pkgs.jq}/bin/jq -r 'to_entries[] | [.key, .value] | @tsv' \
      | while read -r host builder; do
        gcroot=${shell.escape profileRoot}/$host-builder
        ${pkgs.nix}/bin/nix-env -p "$gcroot" --set "$builder"
      done
    echo "$js"
  '';

  profileRoot = "/nix/var/nix/profiles/ci";

  imp = {
    krebs.buildbot.master = {
      slaves = {
        testslave = "lasspass";
      };

      change_source = mapAttrs' (name: repo:
        nameValuePair name (concatMapStrings (url: ''
          cs.append(
              changes.GitPoller(
                  "${url}",
                  workdir='${name}-${elemAt(splitString "." url) 1}', branches=True,
                  project='${name}',
                  pollinterval=10
              )
          )
        '') repo.urls)
      ) cfg.repos;

      scheduler = mapAttrs' (name: repo:
        nameValuePair name ''
          sched.append(
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
              )
          )
          sched.append(
              schedulers.ForceScheduler(
                  name="${name}",
                  builderNames=[
                      "${name}",
                  ]
              )
          )
        ''
      ) cfg.repos;
      builder_pre = ''
        from buildbot import interfaces
        from buildbot.steps.shell import ShellCommand

        class StepToStartMoreSteps(ShellCommand):
            def __init__(self, **kwargs):
                ShellCommand.__init__(self, **kwargs)

            def addBuildSteps(self, steps_factories):
                for sf in steps_factories:
                    step = interfaces.IBuildStepFactory(sf).buildStep()
                    step.setBuild(self.build)
                    step.setBuildSlave(self.build.slavebuilder.slave)
                    step_status = self.build.build_status.addStepWithName(step.name)
                    step.setStepStatus(step_status)
                    self.build.steps.append(step)

            def start(self):
                props = self.build.getProperties()
                new_steps = json.loads(props.getProperty('steps_json'))
                for new_step in new_steps:
                    self.addBuildSteps([steps.ShellCommand(
                        name=str(new_step),
                        command=[
                          "${pkgs.writeDash "build-stepper.sh" ''
                            set -efu
                            profile=${shell.escape profileRoot}/$build_name
                            result=$("$build_script")
                            ${pkgs.nix}/bin/nix-env -p "$profile" --set "$result"
                          ''}"
                        ],
                        env={
                          "build_name": new_step,
                          "build_script": new_steps[new_step],
                          "NIX_REMOTE": "daemon",
                          "NIX_PATH": "secrets=/var/src/stockholm/null:/var/src",
                        },
                        timeout=90001,
                        workdir='build', # TODO figure out why we need this?
                    )])

                ShellCommand.start(self)

      '';

      builder = mapAttrs' (name: repo:
        nameValuePair name ''
          f_${name} = util.BuildFactory()
          f_${name}.addStep(steps.Git(
              repourl=util.Property('repository', '${head repo.urls}'),
              mode='full',
              submodules=True,
          ))

          f_${name}.addStep(steps.SetPropertyFromCommand(
              env={
                "NIX_REMOTE": "daemon",
                "NIX_PATH": "secrets=/var/src/stockholm/null:/var/src",
              },
              name="get_steps",
              command=["${getJobs}"],
              extract_fn=lambda rc, stdout, stderr: { 'steps_json': stdout },
          ))
          f_${name}.addStep(StepToStartMoreSteps(command=["echo"])) # TODO remove dummy command from here

          bu.append(
              util.BuilderConfig(
                  name="${name}",
                  slavenames=slavenames,
                  factory=f_${name}
              )
          )
        ''
      ) cfg.repos;

      enable = true;
      web.enable = true;
      irc = {
        enable = true;
        nick = "build|${hostname}";
        server = "irc.r";
        channels = [ "xxx" "noise" ];
        allowForce = true;
      };
      extraConfig = ''
        c['buildbotURL'] = "http://build.${hostname}.r/"
      '';
    };

    krebs.buildbot.slave = {
      enable = true;
      masterhost = "localhost";
      username = "testslave";
      password = "lasspass";
      packages = with pkgs; [ gnumake jq nix populate gnutar lzma gzip ];
    };

    system.activationScripts.buildbots-nix-profile = ''
      ${pkgs.coreutils}/bin/mkdir -p ${shell.escape profileRoot}
      ${pkgs.coreutils}/bin/chmod 0770 ${shell.escape profileRoot}
      ${pkgs.coreutils}/bin/chgrp buildbots ${shell.escape profileRoot}
    '';

    users = {
      groups.buildbots.gid = genid "buildbots";
      users = {
        buildbotMaster.extraGroups = [ "buildbots" ];
        buildbotSlave.extraGroups = [ "buildbots" ];
      };
    };
  };

in out

