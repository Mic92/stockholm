{ lib, config, pkgs, ... }:
let
    pkgs-unst = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  nixpkgs.config.packageOverrides = pkgs: {
    buildbot = pkgs-unst.buildbot;
    buildbot-slave = pkgs-unst.buildbot-slave;
  };
  networking.firewall.allowedTCPPorts = [ 8010 9989 ];
  krebs.buildbot.master = {
    secrets = [ "retiolum-ci.rsa_key.priv" "cac.json" ];
    slaves = {
      testslave =  "krebspass";
    };
    change_source.stockholm = ''
  stockholm_repo = 'http://cgit.gum/stockholm'
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
                              builderNames=["full-tests"]))
        '';
        fast-tests-scheduler = ''
  # test the master real quick
  sched.append(schedulers.SingleBranchScheduler(
                              change_filter=util.ChangeFilter(branch="master"),
                              name="fast-master-test",
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
                              name="full-master-test",
                              builderNames=["full-tests"]))
        '';
    };
    builder_pre = ''
  # prepare grab_repo step for stockholm
  stockholm_repo = "http://cgit.gum.retiolum/stockholm"
  grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')

  env = {"LOGNAME": "shared", "NIX_REMOTE": "daemon"}

  # prepare nix-shell
  # the dependencies which are used by the test script
  deps = [ "gnumake", "jq","nix","rsync",
            "(import <stockholm> {}).pkgs.test.infest-cac-centos7" ]
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
  addShell(f,name="deploy-eval-centos7",env=env,
            command=nixshell + ["make -s eval get=krebs.deploy filter=json system=test-centos7"])

  addShell(f,name="deploy-eval-wolf",env=env,
            command=nixshell + ["make -s eval get=krebs.deploy filter=json system=wolf"])

  addShell(f,name="deploy-eval-cross-check",env=env,
            command=nixshell + ["! make eval get=krebs.deploy filter=json system=test-failing"])

  addShell(f,name="instantiate-test-all-modules",env=env,
            command=nixshell + \
                      ["touch retiolum.rsa_key.priv; \
                        nix-instantiate --eval -A \
                            users.shared.test-all-krebs-modules.system \
                            -I stockholm=. \
                            -I secrets=. '<stockholm>' \
                            --argstr current-date lol \
                            --argstr current-user-name shared \
                            --argstr current-host-name lol \
                            --strict --json"])

  addShell(f,name="instantiate-test-minimal-deploy",env=env,
            command=nixshell + \
                      ["nix-instantiate --eval -A \
                            users.shared.test-minimal-deploy.system \
                            -I stockholm=. \
                            -I secrets=. '<stockholm>' \
                            --argstr current-date lol \
                            --argstr current-user-name shared \
                            --argstr current-host-name lol \
                            --strict --json"])

  bu.append(util.BuilderConfig(name="fast-tests",
        slavenames=slavenames,
        factory=f))
      '';
      slow-tests = ''
  s = util.BuildFactory()
  s.addStep(grab_repo)

  # slave needs 2 files:
  # * cac.json
  # * retiolum
  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/cac.json", slavedest="cac.json"))
  s.addStep(steps.FileDownload(mastersrc="${config.krebs.buildbot.master.workDir}/retiolum-ci.rsa_key.priv", slavedest="retiolum.rsa_key.priv"))

  addShell(s, name="infest-cac-centos7",env=env,
              sigtermTime=60,           # SIGTERM 1 minute before SIGKILL
              timeout=7200,             # 2h
              command=nixshell + ["infest-cac-centos7"])

  bu.append(util.BuilderConfig(name="full-tests",
        slavenames=slavenames,
        factory=s))
      '';
    };
    enable = true;
    web = {
      enable = true;
    };
    irc = {
      enable = true;
      nick = "shared-buildbot";
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
    packages = with pkgs;[ git nix ];
    # all nix commands will need a working nixpkgs installation
    extraEnviron = { NIX_PATH="nixpkgs=${toString <nixpkgs>}"; };
  };
}
