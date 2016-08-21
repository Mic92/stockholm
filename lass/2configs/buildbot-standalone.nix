{ lib, config, pkgs, ... }:

with config.krebs.lib;

let
  sshHostConfig = pkgs.writeText "ssh-config" ''
    ControlMaster auto
    ControlPath /tmp/%u_sshmux_%r@%h:%p
    ControlPersist 4h
  '';

in {
  config.krebs.buildbot.master = let
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
                                    treeStableTimer=10,
                                    name="fast-all-branches",
                                    builderNames=["fast-tests"]))
      '';
      build-scheduler = ''
        # build all hosts
        sched.append(schedulers.SingleBranchScheduler(
                                    change_filter=util.ChangeFilter(branch_re=".*"),
                                    treeStableTimer=10,
                                    name="build-all-branches",
                                    builderNames=["build-all", "build-pkgs"]))
      '';
    };
    builder_pre = ''
      # prepare grab_repo step for stockholm
      grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')

      # TODO: get nixpkgs/stockholm paths from krebs
      env_lass = {
        "LOGNAME": "lass",
        "NIX_REMOTE": "daemon",
        "dummy_secrets": "true",
      }
      env_makefu = {
        "LOGNAME": "makefu",
        "NIX_REMOTE": "daemon",
        "dummy_secrets": "true",
      }
      env_shared = {
        "LOGNAME": "shared",
        "NIX_REMOTE": "daemon",
        "dummy_secrets": "true",
      }

      # prepare nix-shell
      # the dependencies which are used by the test script
      deps = [ "gnumake", "jq", "nix", "(import <stockholm>).pkgs.populate", "openssh" ]
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
        for i in [ "mors", "uriel", "shodan", "helios", "cloudkrebs", "echelon", "dishfire", "prism" ]:
          addShell(f,name="build-{}".format(i),env=env_lass,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=build \
                            system={}".format(i)])

        for i in [ "x", "wry", "vbob", "wbob", "shoney" ]:
          addShell(f,name="build-{}".format(i),env=env_makefu,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=build \
                            system={}".format(i)])

        bu.append(util.BuilderConfig(name="build-all",
              slavenames=slavenames,
              factory=f))

      '';

      fast-tests = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        for i in [ "mors", "uriel", "shodan", "helios", "cloudkrebs", "echelon", "dishfire", "prism" ]:
          addShell(f,name="build-{}".format(i),env=env_lass,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=eval \
                            system={}".format(i)])

        for i in [ "x", "wry", "vbob", "wbob", "shoney" ]:
          addShell(f,name="build-{}".format(i),env=env_makefu,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=eval \
                            system={}".format(i)])

        for i in [ "test-minimal-deploy", "test-all-krebs-modules", "wolf" ]:
          addShell(f,name="build-{}".format(i),env=env_shared,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=eval \
                            system={}".format(i)])

        bu.append(util.BuilderConfig(name="fast-tests",
              slavenames=slavenames,
              factory=f))
      '';
      build-pkgs = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        for i in [
          "apt-cacher-ng",
          "bepasty-client-cli",
          "cac-api",
          "cac-cert",
          "cac-panel",
          "charybdis",
          "collectd-connect-time",
          "dic",
          "drivedroid-gen-repo",
          "exim",
          "fortclientsslvpn",
          "get",
          "git-hooks",
          "github-hosts-sync",
          "go",
          "hashPassword",
          "haskellPackages.blessings",
          "haskellPackages.email-header",
          "haskellPackages.megaparsec",
          "haskellPackages.scanner",
          "haskellPackages.xmonad-stockholm",
          "krebspaste",
          "krebszones",
          "logf",
          "much",
          "newsbot-js",
          "noVNC",
          "passwdqc-utils",
          "populate",
          "posix-array",
          "pssh",
          "push",
          "Reaktor",
          "realwallpaper",
          "repo-sync",
          "retiolum-bootstrap",
          "tarantool",
          "test",
          "tinc_graphs",
          "translate-shell",
          "urlwatch",
          "with-tmpdir",
          "youtube-tools",
        ]:
          addShell(f,name="build-{}".format(i),env=env_lass,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make system=prism pkgs.{}".format(i)])

        bu.append(util.BuilderConfig(name="build-pkgs",
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

  config.krebs.buildbot.slave = {
    enable = true;
    masterhost = "localhost";
    username = "testslave";
    password = "lasspass";
    packages = with pkgs; [ gnumake jq nix populate ];
    extraEnviron = {
      NIX_PATH="/var/src";
    };
  };
  config.krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8010"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport 9989"; target = "ACCEPT"; }
      ];
    };
  };

  #ssh workaround for make test
  options.lass.build-ssh-privkey = mkOption {
    type = types.secret-file;
    default = {
      path = "${config.users.users.buildbotSlave.home}/ssh.privkey";
      owner = { inherit (config.users.users.buildbotSlave ) name uid;};
      source-path = toString <secrets> + "/build.ssh.key";
    };
  };
  config.krebs.secret.files = {
    build-ssh-privkey = config.lass.build-ssh-privkey;
  };
  config.users.users = {
    build = {
      name = "build";
      uid = genid "build";
      home = "/home/build";
      useDefaultShell = true;
      createHome = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDiV0Xn60aVLHC/jGJknlrcxSvKd/MVeh2tjBpxSBT3II9XQGZhID2Gdh84eAtoWyxGVFQx96zCHSuc7tfE2YP2LhXnwaxHTeDc8nlMsdww53lRkxihZIEV7QHc/3LRcFMkFyxdszeUfhWz8PbJGL2GYT+s6CqoPwwa68zF33U1wrMOAPsf/NdpSN4alsqmjFc2STBjnOd9dXNQn1VEJQqGLG3kR3WkCuwMcTLS5eu0KLwG4i89Twjy+TGp2QsF5K6pNE+ZepwaycRgfYzGcPTn5d6YQXBgcKgHMoSJsK8wqpr0+eFPCDiEA3HDnf76E4mX4t6/9QkMXCLmvs0IO/WP lass@mors"
      ];
    };
  };
}
