{ config, pkgs, lib, ... }:

with import <stockholm/lib>;
let
  buildbot-master-config = pkgs.writeText "buildbot-master.cfg" ''
    # -*- python -*-
    from buildbot.plugins import *
    import re
    import json
    c = BuildmasterConfig = {}

    c['slaves'] = []
    slaves = json.loads('${builtins.toJSON cfg.slaves}')
    slavenames = [ s for s in slaves ]
    for k,v in slaves.items():
      c['slaves'].append(buildslave.BuildSlave(k, v))

    # TODO: configure protocols?
    c['protocols'] = {'pb': {'port': 9989}}

    ####### Build Inputs
    c['change_source'] = cs = []

    ${ concatStringsSep "\n"
    (mapAttrsToList (n: v: ''
        #### Change_Source: Begin of ${n}
        ${v}
        #### Change_Source: End of ${n}
      '') cfg.change_source )}

    ####### Build Scheduler
    c['schedulers'] = sched = []

    ${ concatStringsSep "\n"
    (mapAttrsToList (n: v: ''
        #### Schedulers: Begin of ${n}
        ${v}
        #### Schedulers: End of ${n}
      '') cfg.scheduler )}

    ###### Builder
    c['builders'] = bu = []
    
    # Builder Pre: Begin
    ${cfg.builder_pre}
    # Builder Pre: End

    ${ concatStringsSep "\n"
    (mapAttrsToList (n: v: ''
        #### Builder: Begin of ${n}
        ${v}
        #### Builder: End of ${n}
      '') cfg.builder )}


    ####### Status
    c['status'] = st = []

    # If you want to configure this url, override with extraConfig
    c['buildbotURL'] = "http://${config.networking.hostName}:${toString cfg.web.port}/"

    ${optionalString (cfg.web.enable) ''
      from buildbot.status import html
      from buildbot.status.web import authz, auth
      authz_cfg=authz.Authz(
          auth=auth.BasicAuth([ ("${cfg.web.username}","${cfg.web.password}") ]),
          # TODO: configure harder
          gracefulShutdown = False,
          forceBuild = 'auth',
          forceAllBuilds = 'auth',
          pingBuilder = False,
          stopBuild = 'auth',
          stopAllBuilds = 'auth',
          cancelPendingBuild = 'auth'
      )
      # TODO: configure krebs.nginx
      st.append(html.WebStatus(http_port=${toString cfg.web.port}, authz=authz_cfg))
      ''}

    ${optionalString (cfg.irc.enable) ''
      from buildbot.status import words
      irc = words.IRC("${cfg.irc.server}", "${cfg.irc.nick}",
                      channels=${builtins.toJSON cfg.irc.channels},
                      notify_events={
                        'started': 1,
                        'success': 1,
                        'failure': 1,
                        'exception': 1,
                        'successToFailure': 1,
                        'failureToSuccess': 1,
                      }${optionalString cfg.irc.allowForce ",allowForce=True"})
      c['status'].append(irc)
      ''}

    ${ concatStringsSep "\n"
    (mapAttrsToList (n: v: ''
        #### Status: Begin of ${n}
        ${v}
        #### Status: End of ${n}
      '') cfg.status )}

    ####### PROJECT IDENTITY
    c['title'] = "${cfg.title}"
    c['titleURL'] = "http://krebsco.de"


    ####### DB URL
    # TODO: configure
    c['db'] = {
        'db_url' : "sqlite:///state.sqlite",
    }
    ${cfg.extraConfig}
    '';

  cfg = config.krebs.buildbot.master;

  api = {
    enable = mkEnableOption "Buildbot Master";
    title = mkOption {
      default = "Buildbot CI";
      type = types.str;
      description = ''
        Title of the Buildbot Installation
      '';
    };
    workDir = mkOption {
      default = "/var/lib/buildbot/master";
      type = types.str;
      description = ''
        Path to build bot master directory.
        Will be created on startup.
      '';
    };

    secrets = mkOption {
      default = [];
      type = types.listOf types.str;
      example = [ "cac.json" ];
      description = ''
        List of all the secrets in ‹secrets› which should be copied into the
        buildbot master directory.
      '';
    };

    slaves = mkOption {
      default = {};
      type = types.attrsOf types.str;
      description = ''
        Attrset of slavenames with their passwords
        slavename = slavepassword
      '';
    };

    change_source = mkOption {
      default = {};
      type = types.attrsOf types.str;
      example = {
        stockholm = ''
          cs.append(changes.GitPoller(
                  'http://cgit.gum/stockholm',
                  workdir='stockholm-poller', branch='master',
                  project='stockholm',
                  pollinterval=120))
        '';
      };
      description = ''
        Attrset of all the change_sources which should be configured.
        It will be directly included into the master configuration.

        At the end an change object should be appended to <literal>cs</literal>
      '';
    };

    scheduler = mkOption {
      default = {};
      type = types.attrsOf types.str;
      example = {
        force-scheduler = ''
          sched.append(schedulers.ForceScheduler(
                                      name="force",
                                      builderNames=["full-tests"]))
        '';
      };
      description = ''
        Attrset of all the schedulers which should be configured.
        It will be directly included into the master configuration.

        At the end an change object should be appended to <literal>sched</literal>
      '';
    };

    builder_pre = mkOption {
      default = "";
      type = types.lines;
      example = ''
        grab_repo = steps.Git(repourl=stockholm_repo, mode='incremental')
      '';
      description = ''
        some code before the builders are being assembled.
        can be used to define functions used by multiple builders
      '';
    };

    builder = mkOption {
      default = {};
      type = types.attrsOf types.str;
      example = {
        fast-test = ''
        '';
      };
      description = ''
        Attrset of all the builder which should be configured.
        It will be directly included into the master configuration.

        At the end an change object should be appended to <literal>bu</literal>
      '';
    };

    status = mkOption {
      default = {};
      type = types.attrsOf types.str;
      description = ''
        Attrset of all the extra status which should be configured.
        It will be directly included into the master configuration.

        At the end an change object should be appended to <literal>st</literal>

        Right now IRC and Web status can be configured by setting
        <literal>buildbot.master.irc.enable</literal> and
        <literal>buildbot.master.web.enable</literal>
      '';
    };

    # Configurable Stati
    web = mkOption {
      default = {};
      type = types.submodule ({ config2, ... }: {
        options = {
          enable = mkEnableOption "Buildbot Master Web Status";
          username = mkOption {
            default = "krebs";
            type = types.str;
            description = ''
              username for web authentication
            '';
          };
          hostname = mkOption {
            default = config.networking.hostName;
            type = types.str;
            description = ''
              web interface Hostname
            '';
          };
          password = mkOption {
            default = "bob";
            type = types.str;
            description = ''
              password for web authentication
            '';
          };
          port = mkOption {
            default = 8010;
            type = types.int;
            description = ''
              port for buildbot web status
            '';
          };
        };
      });
    };

    irc = mkOption {
      default = {};
      type = types.submodule ({ config, ... }: {
        options = {
          enable = mkEnableOption "Buildbot Master IRC Status";
          channels = mkOption {
            default = [ "nix-buildbot-meetup" ];
            type = with types; listOf str;
            description = ''
              irc channels the bot should connect to
            '';
          };
          allowForce = mkOption {
            default = false;
            type = types.bool;
            description = ''
              Determines if builds can be forced via IRC
            '';
          };
          nick = mkOption {
            default = "nix-buildbot";
            type = types.str;
            description = ''
              nickname for IRC
            '';
          };
          server = mkOption {
            default = "irc.freenode.net";
            type = types.str;
            description = ''
              Buildbot Status IRC Server to connect to
            '';
          };
        };
      });
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      description = ''
        extra config appended to the generated master.cfg
      '';
    };
  };

  imp = {

    users.extraUsers.buildbotMaster = {
      uid = genid "buildbotMaster";
      description = "Buildbot Master";
      home = cfg.workDir;
      createHome = false;
      isSystemUser = true;
    };

    users.extraGroups.buildbotMaster = {
      gid = 672626386;
    };

    systemd.services.buildbotMaster = {
      description = "Buildbot Master";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      # TODO: add extra dependencies to master like svn and cvs
      path = [ pkgs.git ];
      environment = {
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };
      serviceConfig = let
        workdir = shell.escape cfg.workDir;
        secretsdir = shell.escape (toString <secrets>);
      in {
        PermissionsStartOnly = true;
        # TODO: maybe also prepare buildbot.tac?
        ExecStartPre = pkgs.writeDash "buildbot-master-init" ''
          set -efux
          if [ ! -e ${workdir} ];then
            mkdir -p ${workdir}
            ${pkgs.buildbot-classic}/bin/buildbot create-master -r -l 10 -f ${workdir}
          fi
          # always override the master.cfg
          cp ${buildbot-master-config} ${workdir}/master.cfg

          # copy secrets
          ${ concatMapStringsSep "\n"
            (f: "cp ${secretsdir}/${f} ${workdir}/${f}" ) cfg.secrets }
          # sanity
          ${pkgs.buildbot-classic}/bin/buildbot checkconfig ${workdir}

          # TODO: maybe upgrade? not sure about this
          #       normally we should write buildbot.tac by our own
          # ${pkgs.buildbot-classic}/bin/buildbot upgrade-master ${workdir}

          chmod 700 ${workdir}
          chown buildbotMaster:buildbotMaster -R ${workdir}
        '';
        ExecStart = "${pkgs.buildbot-classic}/bin/buildbot start --nodaemon ${workdir}";
        PrivateTmp = "true";
        User = "buildbotMaster";
        Restart = "always";
        RestartSec = "10";
      };
    };
  };
in
{
  options.krebs.buildbot.master = api;
  config = lib.mkIf cfg.enable imp;
}
