{ lib, config, pkgs, ... }:
# The buildbot config is self-contained and currently provides a way 
# to test "shared" configuration (infrastructure to be used by every krebsminister).

# You can add your own test, test steps as required. Deploy the config on a
# shared host like wolf and everything should be fine.

# TODO for all users schedule a build for fast tests
{
  # due to the fact that we actually build stuff on the box via the daemon,
  # /nix/store should be cleaned up automatically as well
  nix.gc.automatic = true;
  nix.gc.dates = "05:23";

  networking.firewall.allowedTCPPorts = [ 8010 9989 ];
  krebs.buildbot.master = let
    stockholm-mirror-url = http://cgit.wolf/stockholm-mirror ;
  in {
    secrets = [ "retiolum-ci.rsa_key.priv" "cac.json" ];
    slaves = {
      testslave =  "krebspass";
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
  sched.append(schedulers.SingleBranchScheduler(
                              ## all branches
                              change_filter=util.ChangeFilter(branch_re=".*"),
                              treeStableTimer=10,
                              name="fast-all-branches",
                              builderNames=["fast-tests"]))
        '';
        test-cac-infest-master = ''
  # files everyone depends on or are part of the share branch
  def shared_files(change):
    r =re.compile("^((krebs|shared)/.*|Makefile|default.nix)")
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

  env = {"LOGNAME": "shared", "NIX_REMOTE": "daemon"}

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

        for i in [ "test-minimal-deploy", "test-all-krebs-modules", "wolf", "test-centos7" "test-failing" ]:
          addShell(f,name="build-{}".format(i),env=env,
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
      # this build will try to build against local nixpkgs
      # TODO change to do a 'local' populate and use the retrieved nixpkgs
      build-local = ''
  f = util.BuildFactory()
  f.addStep(grab_repo)

  addShell(f,name="build-test-all-modules",env=env,
            command=nixshell + \
                      ["touch retiolum.rsa_key.priv; \
                        nix-build \
                            --show-trace --no-out-link \
                            -I nixos-config=./shared/1systems/test-all-krebs-modules.nix  \
                            -I secrets=. \
                            -A config.system.build.toplevel"]
          )

  bu.append(util.BuilderConfig(name="build-local",
        slavenames=slavenames,
        factory=f))
      '';
#      slow-tests = ''
#  s = util.BuildFactory()
#  s.addStep(grab_repo)
#
#  # slave needs 2 files:
#  # * cac.json
#  # * retiolum
#  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/cac.json", slavedest="cac.json"))
#  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/retiolum-ci.rsa_key.priv", slavedest="retiolum.rsa_key.priv"))
#  addShell(s, name="infest-cac-centos7",env=env,
#              sigtermTime=60,           # SIGTERM 1 minute before SIGKILL
#              timeout=10800,             # 3h
#              command=nixshell + ["infest-cac-centos7"])
#
#  bu.append(util.BuilderConfig(name="full-tests",
#        slavenames=slavenames,
#        factory=s))
#      '';
    };
    enable = true;
    web = {
      enable = true;
    };
    irc = {
      enable = true;
      nick = "wolfbot";
      server = "cd.retiolum";
      channels = [ "retiolum" ];
      allowForce = true;
    };
  };

  krebs.buildbot.slave = {
    enable = true;
    masterhost = "localhost";
    username = "testslave";
    password = "krebspass";
    packages = with pkgs; [ gnumake jq nix populate ];
    # all nix commands will need a working nixpkgs installation
    extraEnviron = {
      NIX_PATH="nixpkgs=/var/src/nixpkgs:nixos-config=./shared/1systems/wolf.nix"; };
  };
}
