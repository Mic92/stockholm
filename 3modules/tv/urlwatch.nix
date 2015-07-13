{ config, lib, pkgs, ... }:

# TODO multiple users
# TODO inform about unused caches
# cache = url: "${cfg.dataDir}/.urlwatch/cache/${hashString "sha1" url}"
# TODO hooks.py

with builtins;
with lib;
let
  cfg = config.tv.urlwatch;

  api = {
    enable = mkEnableOption "tv.urlwatch";

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/urlwatch";
      description = ''
        Directory where the urlwatch service should store its state.
      '';
    };
    from = mkOption {
      type = types.str;
      default = "${cfg.user}@${config.networking.hostName}.retiolum";
      description = ''
        Content of the From: header of the generated mails.
      '';
    };
    mailto = mkOption {
      type = types.str;
      description = ''
        Content of the To: header of the generated mails. [AKA recipient :)]
      '';
    };
    onCalendar = mkOption {
      type = types.str;
      description = ''
        Run urlwatch at this interval.
        The format is described in systemd.time(7), CALENDAR EVENTS.
      '';
      example = "04:23";
    };
    urls = mkOption {
      type = with types; listOf str;
      description = "URL to watch.";
      example = [
        https://nixos.org/channels/nixos-unstable/git-revision
      ];
    };
    user = mkOption {
      type = types.str;
      default = "urlwatch";
      description = "User under which urlwatch runs.";
    };
  };

  urlsFile = toFile "urls" (concatStringsSep "\n" cfg.urls);

  impl = {
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
        User = cfg.user;
        PermissionsStartOnly = "true";
        PrivateTmp = "true";
        Type = "oneshot";
        ExecStartPre =
          pkgs.writeScript "urlwatch-prestart" ''
            #! /bin/sh
            set -euf

            dataDir=$HOME
            user=${escapeShellArg cfg.user}

            if ! test -e "$dataDir"; then
              mkdir -m 0700 -p "$dataDir"
              chown "$user": "$dataDir"
            fi
          '';
        ExecStart = pkgs.writeScript "urlwatch" ''
          #! /bin/sh
          set -euf

          from=${escapeShellArg cfg.from}
          mailto=${escapeShellArg cfg.mailto}
          urlsFile=${escapeShellArg urlsFile}
          user=${escapeShellArg cfg.user}

          cd /tmp

          urlwatch -e --urls="$urlsFile" > changes 2>&1 || :

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
    users.extraUsers = optionals (cfg.user == "urlwatch") (singleton {
      name = "urlwatch";
      uid = 3450919516; # bin/genid urlwatch
    });
  };

in

{
  # TODO
  #imports = [
  #  ./exim
  #];
  #config = mkIf cfg.enable
  #  (if config.tv.exim.enable
  #    then impl
  #    else throw "tv.exim must be enabled when enabling tv.urlwatch");

  options.tv.urlwatch = api;
  
  config = impl;
}
