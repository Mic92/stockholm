{ config, pkgs, lib, ... }:
with import <stockholm/lib>;
let
  domain = "pad.lassul.us";
in
{

  # redirect legacy domain to new one
  services.nginx.virtualHosts."codi.lassul.us" = {
    enableACME = true;
    addSSL = true;
    locations."/".return = "301 https://${domain}\$request_uri";
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "https://localhost:3091";
      proxyWebsockets = true;
    };
  };

  security.acme.certs.${domain}.group = "hedgecert";
  users.groups.hedgecert.members = [ "hedgedoc" "nginx" ];

  security.dhparams = {
    enable = true;
    params.hedgedoc = { };
  };

  systemd.services.hedgedoc.environment = {
    CMD_COOKIE_POLICY = "none";
    CMD_CSP_ALLOW_FRAMING = "true";
  };

  systemd.services.hedgedoc-backup = {
    startAt = "daily";
    serviceConfig = {
      ExecStart = ''${pkgs.sqlite}/bin/sqlite3 /var/lib/hedgedoc/db.hedgedoc.sqlite ".backup /var/backup/hedgedoc/backup.sq3"'';
      Type = "oneshot";
    };
  };

  services.postgresqlBackup.enable = true;

  systemd.services.borgbackup-job-hetzner.serviceConfig.ReadWritePaths = [ "/var/log/telegraf" ];

  services.borgbackup.jobs.hetzner = {
    paths = [
      "/home"
      "/etc"
      "/var"
      "/root"
    ];
    exclude = [
      "*.pyc"
      "/home/*/.direnv"
      "/home/*/.cache"
      "/home/*/.cargo"
      "/home/*/.npm"
      "/home/*/.m2"
      "/home/*/.gradle"
      "/home/*/.opam"
      "/home/*/.clangd"
      "/var/lib/containerd"
      # already included in database backup
      "/var/lib/postgresql"
      # not so important
      "/var/lib/docker/"
      "/var/log/journal"
      "/var/cache"
      "/var/tmp"
      "/var/log"
    ];
    repo = "u348918@u348918.your-storagebox.de:/./hetzner";
    encryption.mode = "none";
    compression = "auto,zstd";
    startAt = "daily";
    # TODO: change backup key
    environment.BORG_RSH = "ssh -oPort=23 -i ${config.sops.secrets.hetzner-borgbackup-ssh.path}";
    preHook = ''
      set -x
    '';

    postHook = ''
      cat > /var/log/telegraf/borgbackup-job-hetzner.service <<EOF
      task,frequency=daily last_run=$(date +%s)i,state="$([[ $exitStatus == 0 ]] && echo ok || echo fail)"
      EOF
    '';

    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
  };

  services.hedgedoc = {
    enable = true;
    configuration.allowOrigin = [ domain ];
    settings = {
      db = {
        dialect = "sqlite";
        storage = "/var/lib/hedgedoc/db.hedgedoc.sqlite";
      };
      useCDN = false;
      port = 3091;
      domain = domain;
      allowFreeURL = true;

      useSSL = true;
      protocolUseSSL = true;
      sslCAPath = [ "/etc/ssl/certs/ca-certificates.crt" ];
      sslCertPath = "/var/lib/acme/${domain}/cert.pem";
      sslKeyPath = "/var/lib/acme/${domain}/key.pem";
      dhParamPath = config.security.dhparams.params.hedgedoc.path;
    };
  };
}
