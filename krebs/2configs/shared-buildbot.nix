{ lib, config, pkgs, ... }:
# The buildbot config is self-contained and currently provides a way
# to test "krebs" configuration (infrastructure to be used by every krebsminister).

# You can add your own test, test steps as required. Deploy the config on a
# krebs host like wolf and everything should be fine.

# TODO for all users schedule a build for fast tests
let
  hostname = config.networking.hostName;
in {
  # due to the fact that we actually build stuff on the box via the daemon,
  # /nix/store should be cleaned up automatically as well
  services.nginx.virtualHosts.build = {
    serverAliases = [ "build.${hostname}.r" ];
    locations."/".extraConfig = ''
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "upgrade";
      proxy_pass http://127.0.0.1:${toString config.krebs.buildbot.master.web.port};
    '';
  };

  nix.gc.automatic = true;
  nix.gc.dates = "05:23";
  networking.firewall.allowedTCPPorts = [ 8010 9989 ];
  krebs.buildbot.master = let
    stockholm-mirror-url = "http://cgit.${hostname}.r/stockholm" ;
  in {
    secrets = [ "retiolum-ci.rsa_key.priv" "cac.json" ];
    workers = {
      testworker =  "krebspass";
    };
    change_source.stockholm = ''
  stockholm_repo = '${stockholm-mirror-url}'
  cs.append(changes.GitPoller(
          stockholm_repo,
          workdir='stockholm-poller', branches=True,
          project='stockholm',
          pollinterval=60))
    '';
    scheduler = {
        force-scheduler = ''
  sched.append(schedulers.ForceScheduler(
                              name="force",
                              builderNames=[
                              #  "full-tests",
                                "fast-tests",
                                "build-local"
                              ]))
        '';
        fast-tests-scheduler = ''
  # test everything real quick
  sched.append(schedulers.AnyBranchScheduler(
                              treeStableTimer=10,
                              name="fast-all-branches",
                              builderNames=["fast-tests"]))
        '';
        test-cac-infest-master = ''
  # files everyone depends on or are part of the share branch
  def shared_files(change):
    r =re.compile("^(krebs/.*|Makefile|default.nix|shell.nix)")
    for file in change.files:
      if r.match(file):
        return True
    return False

  sched.append(schedulers.SingleBranchScheduler(
                              change_filter=util.ChangeFilter(branch="master"),
                              fileIsImportant=shared_files,
                              treeStableTimer=60*60, # master was stable for the last hour
                              name="full-master",
                              builderNames=[
                              #  "full-tests",
                                "build-local"
                              ]))
        '';
    };
    builder_pre = ''
  # prepare grab_repo step for stockholm
  grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')

  env = {
    "LOGNAME": "krebs",
    "NIX_REMOTE": "daemon",
    "dummy_secrets": "true",
  }

  # prepare nix-shell
  # the dependencies which are used by the test script
  deps = [ "gnumake", "jq", "nix",
            "(import <stockholm>).pkgs.populate",
            "(import <stockholm>).pkgs.test.infest-cac-centos7" ]
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
      fast-tests = ''
        f = util.BuildFactory()
        f.addStep(grab_repo)

        for i in [ "test-minimal-deploy", "test-all-krebs-modules", "wolf", "test-centos7" ]:
          addShell(f,name="build-{}".format(i),env=env,
                  command=nixshell + \
                      ["mkdir -p /tmp/testbuild/$LOGNAME && touch /tmp/testbuild/$LOGNAME/.populate; \
                        make \
                            test \
                            target=$LOGNAME@${config.krebs.build.host.name}/tmp/testbuild/$LOGNAME \
                            method=eval \
                            system={}".format(i)])

        bu.append(util.BuilderConfig(name="fast-tests",
              workernames=workernames,
              factory=f))

            '';
      # this build will try to build against local nixpkgs
      # TODO change to do a 'local' populate and use the retrieved nixpkgs
      build-local = ''
  f = util.BuildFactory()
  f.addStep(grab_repo)


  bu.append(util.BuilderConfig(name="build-local",
        workernames=workernames,
        factory=f))
      '';
#      slow-tests = ''
#  s = util.BuildFactory()
#  s.addStep(grab_repo)
#
#  # worker needs 2 files:
#  # * cac.json
#  # * retiolum
#  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/cac.json", workerdest="cac.json"))
#  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/retiolum-ci.rsa_key.priv", workerdest="retiolum.rsa_key.priv"))
#  addShell(s, name="infest-cac-centos7",env=env,
#              sigtermTime=60,           # SIGTERM 1 minute before SIGKILL
#              timeout=10800,             # 3h
#              command=nixshell + ["infest-cac-centos7"])
#
#  bu.append(util.BuilderConfig(name="full-tests",
#        workernames=workernames,
#        factory=s))
#      '';
    };
    enable = true;
    web = {
      enable = true;
    };
    irc = {
      enable = true;
      nick = "${hostname}bot";
      server = "ni.r";
      channels = [ { channel = "retiolum"; } ];
      allowForce = true;
    };
    extraConfig = ''
      c['buildbotURL'] = "http://build.${hostname}.r/"
    '';
  };

  krebs.buildbot.worker = {
    enable = true;
    masterhost = "localhost";
    username = "testworker";
    password = "krebspass";
    packages = with pkgs; [ gnumake jq nix populate ];
    # all nix commands will need a working nixpkgs installation
    extraEnviron = {
      NIX_PATH="nixpkgs=/var/src/nixpkgs:nixos-config=./krebs/1systems/${hostname}/config.nix:stockholm=./"; };
  };
}
