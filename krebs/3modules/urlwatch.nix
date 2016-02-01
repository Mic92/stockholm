{ config, lib, pkgs, ... }:

# TODO multiple users
# TODO inform about unused caches
# cache = url: "${cfg.dataDir}/.urlwatch/cache/${hashString "sha1" url}"
# TODO hooks.py

with lib;
let
  cfg = config.krebs.urlwatch;

  # TODO assert sendmail's existence
  out = {
    options.krebs.urlwatch = api;
    config = mkIf cfg.enable imp;
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
      default = "${user.name}@${config.networking.hostName}.retiolum";
      description = ''
        Content of the From: header of the generated mails.
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
    urls = mkOption {
      type = with types; listOf str;
      default = [];
      description = "URL to watch.";
      example = [
        https://nixos.org/channels/nixos-unstable/git-revision
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

  urlsFile = toFile "urls" (concatMapStringsSep "\n---\n" toJSON cfg.urls);

  configFile = toFile "urlwatch.yaml" (toJSON {
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
      text = {
        details = true;
        footer = true;
        line_length = 75;
      };
    };
  });

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
        Type = "oneshot";
        ExecStartPre =
          pkgs.writeScript "urlwatch-prestart" ''
            #! /bin/sh
            set -euf

            dataDir=$HOME

            if ! test -e "$dataDir"; then
              mkdir -m 0700 -p "$dataDir"
              chown ${user.name}: "$dataDir"
            fi
          '';
        ExecStart = pkgs.writeScript "urlwatch" ''
          #! /bin/sh
          set -euf
          from=${escapeShellArg cfg.from}
          mailto=${escapeShellArg cfg.mailto}
          urlsFile=${escapeShellArg urlsFile}
          configFile=${escapeShellArg configFile}

          cd /tmp

          urlwatch \
              ${optionalString cfg.verbose "-v"} \
              --urls="$urlsFile" \
              --config="$configFile" \
            > changes || :

          if test -s changes; then
            date=$(date -R)
            subject=$(sed -n 's/^\(CHANGED\|ERROR\|NEW\): //p' changes \
              | tr \\n \ )
            {
              echo "Date: $date"
              echo "From: $from"
              echo "Subject: $subject"
              echo "To: $mailto"
              echo
              cat changes
            } | /var/setuid-wrappers/sendmail -t
          fi
        '';
      };
    };
    users.extraUsers = singleton {
      inherit (user) name uid;
    };
  };

  user = rec {
    name = "urlwatch";
    uid = genid name;
  };
in
out
