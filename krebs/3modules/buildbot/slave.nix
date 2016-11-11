{ config, pkgs, lib, ... }:

with import <stockholm/lib>;
let
  default-packages = [ pkgs.git pkgs.bash ];
  buildbot = pkgs.stdenv.lib.overrideDerivation pkgs.buildbot-worker (old:{
    patches = [ ./buildbot-worker.patch ];
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ pkgs.coreutils ];
  });
  cfg = config.krebs.buildbot.worker;

  api = {
    enable = mkEnableOption "Buildbot worker";

    workDir = mkOption {
      default = "/var/lib/buildbot/worker";
      type = types.str;
      description = ''
        Path to build bot worker directory.
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
        workername used to authenticate with master
      '';
    };

    password = mkOption {
      type = types.str;
      description = ''
        worker password used to authenticate with master
      '';
    };

    contact = mkOption {
      default = "nix worker <buildworker@${config.networking.hostName}>";
      type = types.str;
      description = ''
        contact to be announced by buildworker
      '';
    };

    description = mkOption {
      default = "Nix Generated Buildworker";
      type = types.str;
      description = ''
        description for hostto be announced by buildworker
      '';
    };

    packages = mkOption {
      default = [ pkgs.git ];
      type = with types; listOf package;
      description = ''
        packages which should be in path for buildworker
      '';
    };

    extraEnviron = mkOption {
      default = {};
      example = {
        NIX_PATH = "nixpkgs=/path/to/my/nixpkgs";
      };
      type = types.attrsOf types.str;
      description = ''
        extra environment variables to be provided to the buildworker service
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
        extra config evaluated before calling Buildworker init in .tac file
      '';
    };
  };

  imp = {

    users.extraUsers.buildbotworker = {
      uid = genid "buildbotworker";
      description = "Buildbot worker";
      home = cfg.workDir;
      createHome = false;
    };

    users.extraGroups.buildbotworker = {
      gid = genid "buildbotworker";
    };

    systemd.services."buildbotworker-${cfg.username}-${cfg.masterhost}" = {
      description = "Buildbot worker for ${cfg.username}@${cfg.masterhost}";
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
        masterhost = shell.escape cfg.masterhost;
        username = shell.escape cfg.username;
        password = shell.escape cfg.password;
      in {
        PermissionsStartOnly = true;
        Type = "forking";
        PIDFile = "${workdir}/twistd.pid";
        ExecStartPre = pkgs.writeDash "buildbot-slave-init" ''
          set -efux
          mkdir -p ${workdir}/info
          # TODO: cleanup .tac file?
          ${buildbot}/bin/buildbot-worker create-worker ${workdir} ${masterhost} ${username} ${password}
          echo ${contact} > ${workdir}/info/admin
          echo ${description} > ${workdir}/info/host

          chown buildbotworker:buildbotworker -R ${workdir}
          chmod 700 -R ${workdir}
        '';
        ExecStart = "${buildbot}/bin/buildbot-worker start ${workdir}";
        ExecStop = "${buildbot}/bin/buildbot-worker stop ${workdir}";
        PrivateTmp = "true";
        User = "buildbotworker";
        Restart = "always";
        RestartSec = "10";
      };
    };
  };
in
{
  options.krebs.buildbot.worker = api;
  config = lib.mkIf cfg.enable imp;
}
