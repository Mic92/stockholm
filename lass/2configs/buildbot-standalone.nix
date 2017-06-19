{ lib, config, pkgs, ... }:

with import <stockholm/lib>;

let
  sshHostConfig = pkgs.writeText "ssh-config" ''
    ControlMaster auto
    ControlPath /tmp/%u_sshmux_%r@%h:%p
    ControlPersist 4h
  '';

in {
  config.services.nginx.virtualHosts.build = {
    serverAliases = [ "build.prism.r" ];
    locations."/".extraConfig = ''
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "upgrade";
      proxy_pass http://localhost:${toString config.krebs.buildbot.master.web.port};
    '';
  };

  config.krebs.buildbot.master = let
    stockholm-mirror-url = http://cgit.prism.r/stockholm ;
  in {
    workers = {
      testworker = "lasspass";
    };
    change_source.stockholm = ''
      stockholm_repo = '${stockholm-mirror-url}'
      cs.append(
          changes.GitPoller(
              stockholm_repo,
              workdir='stockholm-poller', branches=True,
              project='stockholm',
              pollinterval=120
          )
      )
    '';
    scheduler = {
      build-scheduler = ''
        # build all hosts
        sched.append(
              schedulers.SingleBranchScheduler(
                  change_filter=util.ChangeFilter(branch_re=".*"),
                  treeStableTimer=10,
                  name="build-all-branches",
                  builderNames=["build-hosts"]
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
      env_nin = {
        "LOGNAME": "nin",
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
      deps = [
        "gnumake",
        "jq",
        "nix",
        "(import <stockholm>).pkgs.populate",
        "openssh"
      ]
      # TODO: --pure , prepare ENV in nix-shell command:
      #                   SSL_CERT_FILE,LOGNAME,NIX_REMOTE
      nixshell = [
        "nix-shell",
        "-I", "stockholm=.",
        "-p"
      ] + deps + [ "--run" ]

      # prepare addShell function
      def addShell(factory,**kwargs):
        factory.addStep(steps.ShellCommand(**kwargs))
    '';
    builder = {
      build-hosts = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)
        for i in [ "test-minimal-deploy", "test-all-krebs-modules", "wolf", "test-centos7" ]:
            addShell(f,name="build-{}".format(i),env=env_shared,
                command=nixshell + \
                    ["mkdir -p $HOME/$LOGNAME && touch $HOME/$LOGNAME/.populate; \
                        make NIX_PATH=$HOME/$LOGNAME test method=build \
                            target=buildbotworker@${config.krebs.build.host.name}$HOME/$LOGNAME \
                            system={}".format(i)
                    ]
            )

        for i in [ "mors", "uriel", "shodan", "icarus", "cloudkrebs", "echelon", "dishfire", "prism" ]:
            addShell(f,name="build-{}".format(i),env=env_lass,
                command=nixshell + \
                    ["mkdir -p $HOME/$LOGNAME && touch $HOME/$LOGNAME/.populate; \
                        make NIX_PATH=$HOME/$LOGNAME test method=build \
                            target=buildbotworker@${config.krebs.build.host.name}$HOME/$LOGNAME \
                            system={}".format(i)
                    ]
            )

        for i in [ "x", "wry", "vbob", "wbob", "shoney" ]:
            addShell(f,name="build-{}".format(i),env=env_makefu,
                command=nixshell + \
                    ["mkdir -p $HOME/$LOGNAME && touch $HOME/$LOGNAME/.populate; \
                        make NIX_PATH=$HOME/$LOGNAME test method=build \
                            target=buildbotworker@${config.krebs.build.host.name}$HOME/$LOGNAME \
                            system={}".format(i)
                    ]
            )

        for i in [ "hiawatha", "onondaga" ]:
            addShell(f,name="build-{}".format(i),env=env_nin,
                command=nixshell + \
                    ["mkdir -p $HOME/$LOGNAME && touch $HOME/$LOGNAME/.populate; \
                        make NIX_PATH=$HOME/$LOGNAME test method=build \
                            target=buildbotworker@${config.krebs.build.host.name}$HOME/$LOGNAME \
                            system={}".format(i)
                    ]
            )

        bu.append(
            util.BuilderConfig(
                name="build-hosts",
                workernames=workernames,
                factory=f
            )
        )

      '';
    };
    enable = true;
    web.enable = true;
    irc = {
      enable = true;
      nick = "buildbot-lass";
      server = "ni.r";
      channels = [ { channel = "retiolum"; } { channel = "noise"; } ];
      allowForce = true;
    };
    extraConfig = ''
      c['buildbotURL'] = "http://build.prism.r/"
    '';
  };

  config.krebs.buildbot.worker = {
    enable = true;
    masterhost = "localhost";
    username = "testworker";
    password = "lasspass";
    packages = with pkgs; [ gnumake jq nix populate ];
    extraEnviron = {
      NIX_PATH="/var/src";
    };
  };
  config.krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 9989"; target = "ACCEPT"; }
      ];
    };
  };

  #ssh workaround for make test
  options.lass.build-ssh-privkey = mkOption {
    type = types.secret-file;
    default = {
      path = "${config.users.users.buildbotworker.home}/.ssh/id_rsa";
      owner = { inherit (config.users.users.buildbotworker ) name uid;};
      source-path = toString <secrets> + "/build.ssh.key";
    };
  };
  config.krebs.secret.files = {
    build-ssh-privkey = config.lass.build-ssh-privkey;
  };
  config.users.users.buildbotworker = {
    useDefaultShell = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDiV0Xn60aVLHC/jGJknlrcxSvKd/MVeh2tjBpxSBT3II9XQGZhID2Gdh84eAtoWyxGVFQx96zCHSuc7tfE2YP2LhXnwaxHTeDc8nlMsdww53lRkxihZIEV7QHc/3LRcFMkFyxdszeUfhWz8PbJGL2GYT+s6CqoPwwa68zF33U1wrMOAPsf/NdpSN4alsqmjFc2STBjnOd9dXNQn1VEJQqGLG3kR3WkCuwMcTLS5eu0KLwG4i89Twjy+TGp2QsF5K6pNE+ZepwaycRgfYzGcPTn5d6YQXBgcKgHMoSJsK8wqpr0+eFPCDiEA3HDnf76E4mX4t6/9QkMXCLmvs0IO/WP"
    ];
  };
}
