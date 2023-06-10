{ config, lib, pkgs, ... }:

# TODO multiple users
# TODO inform about unused caches
# cache = url: "${cfg.dataDir}/.urlwatch/cache/${hashString "sha1" url}"

with import ../../lib/pure.nix { inherit lib; };
let
  cfg = config.krebs.urlwatch;

  # TODO assert sendmail's existence
  out = {
    options.krebs.urlwatch = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.urlwatch";
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/urlwatch";
      description = ''
        Directory where the urlwatch service should store its state.
      '';
    };
    from = mkOption {
      type = types.str;
      default = "${user.name}@${config.networking.hostName}.r";
      description = ''
        Content of the From: header of the generated mails.
      '';
    };
    # TODO hooks :: attrsOf hook
    hooksFile = mkOption {
      type = with types; nullOr path;
      default = null;
      description = ''
        File to use as hooks.py module.
      '';
    };
    mailto = mkOption {
      type = types.str;
      default = config.krebs.build.user.mail;
      description = ''
        Content of the To: header of the generated mails. [AKA recipient :)]
      '';
    };
    onCalendar = mkOption {
      type = types.str;
      default = "04:23";
      description = ''
        Run urlwatch at this interval.
        The format is described in systemd.time(7), CALENDAR EVENTS.
      '';
    };
    sendmail.enable = mkEnableOption "krebs.urlwatch.sendmail" // {
      default = true;
    };
    telegram = {
      enable = mkEnableOption "krebs.urlwatch.telegram";
      botToken = mkOption {
        type = types.str;
      };
      chatId = mkOption {
        type = types.listOf types.str;
      };
    };
    urls = mkOption {
      type = with types; listOf (either str subtypes.job);
      default = [];
      description = "URL to watch.";
      example = [
        https://nixos.org/channels/nixos-unstable/git-revision
        { url = http://localhost ; filter = [ (grep "important.*stuff") ]; }
      ];
      apply = map (x: getAttr (typeOf x) {
        set = x;
        string.url = x;
      });
    };
    verbose = mkOption {
      type = types.bool;
      default = false;
      description = ''
        verbose output of urlwatch
      '';
    };
  };

  urlsFile = pkgs.writeText "urls"
    (concatMapStringsSep "\n---\n"
      (x: toJSON (filterAttrs (n: v: n != "_module") x)) cfg.urls);

  hooksFile = cfg.hooksFile;

  configFile = pkgs.writeJSON "urlwatch.yaml" {
    display = {
      error = true;
      new = true;
      unchanged = false;
    };
    report = {
      email = {
        enabled = false;
        from = "";
        html = false;
        smtp = {
          host = "localhost";
          keyring = true;
          port = 25;
          starttls = true;
        };
        subject = "{count} changes: {jobs}";
        to = "";
      };
      html.diff = "unified";
      stdout = {
        color = true;
        enabled = true;
      };
      ${if cfg.telegram.enable then "telegram" else null} = {
        enabled = cfg.telegram.enable;
        bot_token = cfg.telegram.botToken;
        chat_id = cfg.telegram.chatId;
      };
      text = {
        details = true;
        footer = true;
        line_length = 75;
      };
    };
  };

  imp = {
    systemd.timers.urlwatch = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.onCalendar;
        Persistent = "true";
      };
    };
    systemd.services.urlwatch = {
      path = with pkgs; [
        coreutils
        gnused
        urlwatch
      ];
      environment = {
        HOME = cfg.dataDir;
        LC_ALL = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };
      serviceConfig = {
        User = user.name;
        PermissionsStartOnly = "true";
        PrivateTmp = "true";
        SyslogIdentifier = "urlwatch";
        Type = "oneshot";
        ExecStart = pkgs.writeDash "urlwatch" ''
          set -euf

          cd /tmp

          urlwatch \
              ${optionalString cfg.verbose "-v"} \
              --config=${shell.escape configFile} \
              ${optionalString (hooksFile != null)
                "--hooks=${shell.escape hooksFile}"
              } \
              --urls=${shell.escape urlsFile} \
            > changes || :

          ${optionalString cfg.sendmail.enable /* sh */ ''
            if test -s changes; then
              {
                echo Date: $(date -R)
                echo From: ${shell.escape cfg.from}
                echo Subject: $(
                  sed -nr 's/^(CHANGED|ERROR|NEW): //p' changes |
                  sed '1!s/^  //'
                )
                echo To: ${shell.escape cfg.mailto}
                echo Mime-Version: 1.0
                echo Content-Type: text/plain\; charset=UTF-8
                echo Content-Transfer-Encoding: base64
                echo
                base64 changes
              } | /run/wrappers/bin/sendmail -t
            fi
          ''}
        '';
      };
    };
    users.users.${user.name} = {
      inherit (user) uid;
      home = cfg.dataDir;
      createHome = true;
      isSystemUser = true;
      group = user.name;
    };
    users.groups.${user.name} = {};
  };

  user = rec {
    name = "urlwatch";
    uid = genid_uint31 name;
  };

  subtypes.job = types.submodule {
    options = {
      url = mkOption {
        type = types.str;
      };
      filter = mkOption {
        default = null;
        type =
          with types;
          nullOr (either str (listOf (pkgs.formats.json {}).type));
      };
      ignore_cached = mkOption {
        default = null;
        type = with types; nullOr bool;
      };
    };
  };
in out
