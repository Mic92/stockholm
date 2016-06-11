{ lib, config, pkgs, ... }:
{
  krebs.buildbot.master = let
    stockholm-mirror-url = http://cgit.prism/stockholm ;
  in {
    slaves = {
      testslave = "lasspass";
    };
    change_source.stockholm = ''
      stockholm_repo = '${stockholm-mirror-url}'
      cs.append(changes.GitPoller(
              stockholm_repo,
              workdir='stockholm-poller', branches=True,
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
        # test everything real quick
        sched.append(schedulers.SingleBranchScheduler(
                                    ## all branches
                                    change_filter=util.ChangeFilter(branch_re=".*"),
                                    # treeStableTimer=10,
                                    name="fast-all-branches",
                                    builderNames=["fast-tests"]))
      '';
      build-all-scheduler = ''
        # build all lass hosts
        sched.append(schedulers.SingleBranchScheduler(
                                    ## only master
                                    change_filter=util.ChangeFilter(branch_re="master"),
                                    # treeStableTimer=10,
                                    name="prism-master",
                                    builderNames=["build-all"]))
      '';
    };
    builder_pre = ''
      # prepare grab_repo step for stockholm
      grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')

      # TODO: get nixpkgs/stockholm paths from krebs
      env = {"LOGNAME": "lass", "NIX_REMOTE": "daemon", "dummy_secrets": "true"}

      # prepare nix-shell
      # the dependencies which are used by the test script
      deps = [ "gnumake", "jq", "nix", "rsync" ]
      # TODO: --pure , prepare ENV in nix-shell command:
      #                   SSL_CERT_FILE,LOGNAME,NIX_REMOTE
      nixshell = ["nix-shell",
                    "-I", "stockholm=.",
                    "-I", "nixpkgs=/var/src/nixpkgs",
                    "-p" ] + deps + [ "--run" ]

      # prepare addShell function
      def addShell(factory,**kwargs):
        factory.addStep(steps.ShellCommand(**kwargs))
    '';
    builder = {
      build-all = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        #TODO: get hosts via krebs
        for i in [ "mors", "uriel", "shodan", "helios", "cloudkrebs", "echelon", "dishfire", "prism" ]:
          addShell(f,name="build-{}".format(i),env=env,
                  command=nixshell + \
                      ["nix-build \
                            --show-trace --no-out-link \
                            -I nixos-config=./lass/1systems/{}.nix \
                            -I secrets=./lass/2configs/tests/dummy-secrets \
                            -I stockholm=. \
                            -A config.system.build.toplevel".format(i)])

        bu.append(util.BuilderConfig(name="build-all",
              slavenames=slavenames,
              factory=f))

            '';
      fast-tests = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        for i in [ "prism", "mors", "echelon" ]:
          addShell(f,name="populate-{}".format(i),env=env,
                  command=nixshell + \
                            ["{}( make system={} eval.config.krebs.build.populate \
                               | jq -er .)".format("!" if "failing" in i else "",i)])

        addShell(f,name="build-test-minimal",env=env,
                  command=nixshell + \
                            ["nix-instantiate \
                                  --show-trace --eval --strict --json \
                                  -I nixos-config=./shared/1systems/test-minimal-deploy.nix  \
                                  -I secrets=. \
                                  -A config.system.build.toplevel"]
                )

        bu.append(util.BuilderConfig(name="fast-tests",
              slavenames=slavenames,
              factory=f))

            '';
    };
    enable = true;
    web.enable = true;
    irc = {
      enable = true;
      nick = "buildbot-lass";
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
    packages = with pkgs;[ git nix gnumake jq rsync ];
    extraEnviron = {
      NIX_PATH="nixpkgs=/var/src/nixpkgs";
    };
  };
  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8010"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport 9989"; target = "ACCEPT"; }
      ];
    };
  };
}
