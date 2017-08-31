{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  cfg = config.krebs.ci;

  hostname = config.networking.hostName;
in
{
  options.krebs.ci = {
    enable = mkEnableOption "krebs continous integration";
    stockholmSrc = mkOption {
      type = types.str;
      default = "http://cgit.${hostname}.r/stockholm";
    };
    treeStableTimer = mkOption {
      type = types.int;
      default = 10;
      description = "how long to wait until we test changes (in minutes)";
    };
    hosts = mkOption {
      type = types.listOf types.host;
      default = [];
      description = ''
        List of hosts that should be build
      '';
    };
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = true;
      virtualHosts.build = {
        serverAliases = [ "build.${hostname}.r" ];
        locations."/".extraConfig = ''
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
          proxy_pass http://127.0.0.1:${toString config.krebs.buildbot.master.web.port};
        '';
      };
    };

    krebs.buildbot.master = {
      slaves = {
        testslave = "lasspass";
      };
      change_source.stockholm = ''
        stockholm_repo = '${cfg.stockholmSrc}'
        cs.append(
            changes.GitPoller(
                stockholm_repo,
                workdir='stockholm-poller', branches=True,
                project='stockholm',
                pollinterval=10
            )
        )
      '';
      scheduler = {
        build-scheduler = ''
          # build all hosts
          sched.append(
                schedulers.SingleBranchScheduler(
                    change_filter=util.ChangeFilter(branch_re=".*"),
                    treeStableTimer=${toString cfg.treeStableTimer}*60,
                    name="build-all-branches",
                    builderNames=[
                        "build-hosts"
                    ]
                )
          )
        '';
        force-scheduler = ''
          sched.append(
              schedulers.ForceScheduler(
                    name="force",
                    builderNames=[
                        "build-hosts"
                    ]
              )
          )
        '';
      };
      builder_pre = ''
        # prepare grab_repo step for stockholm
        grab_repo = steps.Git(
            repourl=stockholm_repo,
            mode='full'
        )

        # prepare addShell function
        def addShell(factory,**kwargs):
          factory.addStep(steps.ShellCommand(**kwargs))
      '';
      builder = {
        build-hosts = ''
          f = util.BuildFactory()
          f.addStep(grab_repo)

          def build_host(user, host):
              addShell(f,
                  name="{}".format(host),
                  env={
                    "NIX_PATH": "secrets=/var/src/stockholm/null:/var/src",
                    "NIX_REMOTE": "daemon",
                    "dummy_secrets": "true",
                  },
                  command=[
                    "nix-shell", "--run", " ".join(["test",
                      "--user={}".format(user),
                      "--system={}".format(host),
                      "--force-populate",
                      "--target=$LOGNAME@${config.krebs.build.host.name}$HOME/{}".format(user),
                    ])
                  ],
                  timeout=90001
              )

          ${concatMapStringsSep "\n" (host:
             "build_host(\"${host.owner.name}\", \"${host.name}\")"
          ) cfg.hosts}

          bu.append(
              util.BuilderConfig(
                  name="build-hosts",
                  slavenames=slavenames,
                  factory=f
              )
          )

        '';
      };
      enable = true;
      web.enable = true;
      irc = {
        enable = true;
        nick = "build|${hostname}";
        server = "ni.r";
        channels = [ "retiolum" "noise" ];
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
      packages = with pkgs; [ gnumake jq nix populate ];
    };

  };
}
