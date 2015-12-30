{ lib, config, pkgs, ... }:
{
  #networking.firewall.allowedTCPPorts = [ 8010 9989 ];
  krebs.buildbot.master = {
    slaves = {
      testslave = "lasspass";
    };
    change_source.stockholm = ''
      stockholm_repo = 'http://cgit.mors/stockholm'
      cs.append(changes.GitPoller(
              stockholm_repo,
              workdir='stockholm-poller', branch='master',
              project='stockholm',
              pollinterval=120))
    '';
    scheduler = {
      force-scheduler = ''
        sched.append(schedulers.ForceScheduler(
                                    name="force",
                                    builderNames=["fast-tests"]))
      '';
      fast-tests-scheduler = ''
        # test the master real quick
        sched.append(schedulers.SingleBranchScheduler(
                                    change_filter=util.ChangeFilter(branch="master"),
                                    name="fast-master-test",
                                    builderNames=["fast-tests"]))
      '';
    };
    builder_pre = ''
      # prepare grab_repo step for stockholm
      grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')

      env = {"LOGNAME": "lass", "NIX_REMOTE": "daemon"}

      # prepare nix-shell
      # the dependencies which are used by the test script
      deps = [ "gnumake", "jq","nix","rsync" ]
      # TODO: --pure , prepare ENV in nix-shell command:
      #                   SSL_CERT_FILE,LOGNAME,NIX_REMOTE
      nixshell = ["nix-shell", "-I", "stockholm=.", "-p" ] + deps + [ "--run" ]

      # prepare addShell function
      def addShell(factory,**kwargs):
        factory.addStep(steps.ShellCommand(**kwargs))
    '';
    builder = {
      fast-tests = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        addShell(f,name="mors-eval",env=env,
                  command=nixshell + ["make -s eval get=krebs.deploy filter=json system=mors"])

        bu.append(util.BuilderConfig(name="fast-tests",
              slavenames=slavenames,
              factory=f))
      '';
    };
    enable = true;
    web.enable = true;
    irc = {
      enable = true;
      nick = "lass-buildbot";
      server = "cd.retiolum";
      channels = [ "retiolum" ];
      allowForce = true;
    };
  };

  krebs.buildbot.slave = {
    enable = true;
    masterhost = "localhost";
    username = "testslave";
    password = "lasspass";
    packages = with pkgs;[ git nix ];
    extraEnviron = { NIX_PATH="nixpkgs=${toString <nixpkgs>}"; };
  };
}
