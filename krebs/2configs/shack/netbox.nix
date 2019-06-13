{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.docker-compose ];
  virtualisation.docker.enable = true;
  services.nginx = {
    enable = true;
    virtualHosts."netbox.shack".locations."/".proxyPass = "http://localhost:18080";
  };
  # we store the netbox config there:
  # state = [ "/var/lib/netbox" ];
  systemd.services.backup-netbox = {
    after = [ "netbox-docker-compose.service" ];
    startAt = "daily";
    path = with pkgs; [ docker-compose docker gzip coreutils ];
    script = ''
      cd /var/lib/netbox
      mkdir -p backup
      docker-compose exec -T -upostgres postgres pg_dumpall \
        | gzip > backup/netdata_$(date -Iseconds).dump.gz
    '';
  };

  systemd.services.netbox-docker-compose = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "docker.service" ];
    environment.VERSION = "v2.5.13";
    serviceConfig = {
      WorkingDirectory = "/var/lib/netbox";
      # TODO: grep -q NAPALM_SECRET env/netbox.env
      # TODO: grep -q NAPALM_SECRET netbox-netprod-importer/switches.yml
      ExecStartPre = "${pkgs.docker-compose}/bin/docker-compose pull";
      ExecStart = "${pkgs.docker-compose}/bin/docker-compose up";
      Restart = "always";
      RestartSec = "10";
      StartLimitIntervalSec = 60;
      StartLimitBurst = 3;
    };
  };
}
