{ config, pkgs, lib, ... }:

with import <stockholm/lib>;
let
  buildbot-slave-init = pkgs.writeText "buildbot-slave.tac" ''
    import os

    from buildslave.bot import BuildSlave
    from twisted.application import service

    basedir = '${cfg.workDir}'
    rotateLength = 10000000
    maxRotatedFiles = 10

    application = service.Application('buildslave')

    from twisted.python.logfile import LogFile
    from twisted.python.log import ILogObserver, FileLogObserver
    logfile = LogFile.fromFullPath(os.path.join(basedir, "twistd.log"), rotateLength=rotateLength,
                                  maxRotatedFiles=maxRotatedFiles)
    application.setComponent(ILogObserver, FileLogObserver(logfile).emit)

    buildmaster_host = '${cfg.masterhost}'
    # TODO: masterport?
    port = 9989
    slavename = '${cfg.username}'
    passwd = '${cfg.password}'
    keepalive = 600
    usepty = 0
    umask = None
    maxdelay = 300
    allow_shutdown = None

    ${cfg.extraConfig}

    s = BuildSlave(buildmaster_host, port, slavename, passwd, basedir,
                  keepalive, usepty, umask=umask, maxdelay=maxdelay,
                  allow_shutdown=allow_shutdown)
    s.setServiceParent(application)
    '';
  default-packages = [ pkgs.git pkgs.bash ];
  cfg = config.krebs.buildbot.slave;

  api = {
    enable = mkEnableOption "Buildbot Slave";

    workDir = mkOption {
      default = "/var/lib/buildbot/slave";
      type = types.str;
      description = ''
        Path to build bot slave directory.
        Will be created on startup.
      '';
    };

    masterhost = mkOption {
      default = "localhost";
      type = types.str;
      description = ''
        Hostname/IP of the buildbot master
      '';
    };

    username = mkOption {
      type = types.str;
      description = ''
        slavename used to authenticate with master
      '';
    };

    password = mkOption {
      type = types.str;
      description = ''
        slave password used to authenticate with master
      '';
    };

    contact = mkOption {
      default = "nix slave <buildslave@${config.networking.hostName}>";
      type = types.str;
      description = ''
        contact to be announced by buildslave
      '';
    };

    description = mkOption {
      default = "Nix Generated BuildSlave";
      type = types.str;
      description = ''
        description for hostto be announced by buildslave
      '';
    };

    packages = mkOption {
      default = [ pkgs.git ];
      type = with types; listOf package;
      description = ''
        packages which should be in path for buildslave
      '';
    };

    extraEnviron = mkOption {
      default = {};
      example = {
        NIX_PATH = "nixpkgs=/path/to/my/nixpkgs";
      };
      type = types.attrsOf types.str;
      description = ''
        extra environment variables to be provided to the buildslave service
        if you need nixpkgs, e.g. for running nix-shell you can set NIX_PATH here.
      '';
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      example = ''
        port = 443
        keepalive = 600
      '';
      description = ''
        extra config evaluated before calling BuildSlave init in .tac file
      '';
    };
  };

  imp = {

    users.extraUsers.buildbotSlave = {
      uid = genid "buildbotSlave";
      description = "Buildbot Slave";
      home = cfg.workDir;
      createHome = false;
      isSystemUser = true;
    };

    users.extraGroups.buildbotSlave = {
      gid = 1408105834;
    };

    systemd.services."buildbotSlave-${cfg.username}-${cfg.masterhost}" = {
      description = "Buildbot Slave for ${cfg.username}@${cfg.masterhost}";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = default-packages ++ cfg.packages;

      environment = {
          SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          NIX_REMOTE="daemon";
      } // cfg.extraEnviron;

      serviceConfig = let
        workdir = shell.escape cfg.workDir;
        contact = shell.escape cfg.contact;
        description = shell.escape cfg.description;
        # TODO:make this
      in {
        PermissionsStartOnly = true;
        Type = "forking";
        PIDFile = "${workdir}/twistd.pid";
        # TODO: maybe also prepare buildbot.tac?
        ExecStartPre = pkgs.writeDash "buildbot-master-init" ''
          set -efux
          mkdir -p ${workdir}/info
          cp ${buildbot-slave-init} ${workdir}/buildbot.tac
          echo ${contact} > ${workdir}/info/admin
          echo ${description} > ${workdir}/info/host

          chown buildbotSlave:buildbotSlave -R ${workdir}
          chmod 700 ${workdir}
        '';
        ExecStart = "${pkgs.buildbot-classic-slave}/bin/buildslave start ${workdir}";
        ExecStop = "${pkgs.buildbot-classic-slave}/bin/buildslave stop ${workdir}";
        PrivateTmp = "true";
        User = "buildbotSlave";
        Restart = "always";
        RestartSec = "10";
      };
    };
  };
in
{
  options.krebs.buildbot.slave = api;
  config = lib.mkIf cfg.enable imp;
}
