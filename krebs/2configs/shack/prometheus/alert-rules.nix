{ lib,... }:
let
  disk_free_threshold = "10"; # at least this much free disk percentage
in {
  services.prometheus.rules = [(builtins.toJSON
    {
      groups = [
        { name = "shack-env";
          rules = [
            {
              alert = "Wolf RootPartitionFull";
              for = "30m";
              expr = ''(node_filesystem_avail_bytes{alias="wolf.shack",mountpoint="/"} * 100) / node_filesystem_size_bytes{alias="wolf.shack",mountpoint="/"} < ${disk_free_threshold}'';
              labels.severity = "warning";
              annotations.summary = "{{ $labels.alias }} root disk full";
              annotations.url = "http://grafana.shack/d/hb7fSE0Zz/shack-system-dashboard?orgId=1&var-job=node&var-hostname=All&var-node=wolf.shack:9100&var-device=All&var-maxmount=%2F&var-show_hostname=wolf";
              annotations.description = ''The root disk of {{ $labels.alias }} has {{ $value | printf "%.2f" }}% free disk space (Threshold at ${disk_free_threshold}%). CI for deploying new configuration will seize working. Log in to the system and try to clean up the obsolete files on the machine. There are a couple of things you can do:
1. `nix-collect-garbage -d`
2. clean up the shack share folder in `/home/share`
3. check `du -hs /var/ | sort -h`.
4. run `docker system prune`
5. `find /var/lib/containers/news/var/lib/htgen-go/items -mtime +7 -delete;` to clean up the link shortener data
5. If you are really desperate run `du -hs / | sort -h` and go through the folders recursively until you've found something to delete
6. as a last resort the root disk can be expanded via `lvresize -L +10G /dev/pool/root && btrfs filesystem resize max /` '';
            }
            {
              alert = "Puyak RootPartitionFull";
              for = "30m";
              expr = ''(node_filesystem_avail_bytes{alias="puyak.shack",mountpoint="/"} * 100) / node_filesystem_size_bytes{alias="puyak.shack",mountpoint="/"} < ${disk_free_threshold}'';
              labels.severity = "warning";
              annotations.summary = "{{ $labels.alias }} root disk full";
              annotations.url = "http://grafana.shack/d/hb7fSE0Zz/shack-system-dashboard?orgId=1&var-job=node&var-hostname=All&var-node=wolf.shack:9100&var-device=All&var-maxmount=%2F&var-show_hostname=puyak";
              annotations.description = ''The root disk of {{ $labels.alias }} has {{ $value | printf "%.2f" }}% free disk space (Threshold at ${disk_free_threshold}%).Prometheus will not be able to create new alerts and CI for deploying new configuration will also seize working. Log in to the system and run `nix-collect-garbage -d` and if this does not help you can check `du -hs /var/ | sort -h`, run `docker system prune` or if you are really desperate run `du -hs / | sort -h` and go through the folders recursively until you've found something to delete'';
            }
            # wolf.shack is not worth supervising anymore
            {
              alert = "HostDown";
              expr = ''up{alias="infra01.shack"} == 0'';
              for = "5m";
              labels.severity = "page";
              annotations.summary = "Instance {{ $labels.alias }} down for 5 minutes";
              annotations.url = "http://grafana.shack/d/hb7fSE0Zz/shack-system-dashboard?orgId=1&var-job=node&var-hostname=All&var-node=wolf.shack:9100&var-device=All&var-maxmount=%2F&var-show_hostname=wolf";
              annotations.description = ''Host {{ $labels.alias }} went down and has not been reconnected after 5 minutes. This is probably bad news, as the machine runs one of the DNS servers and the power broadcast proxy which is used to be able to turn off the light via puyak as well as the shutdown listener.'';
            }
          ];
        }
      ];
    }
  )];
}
