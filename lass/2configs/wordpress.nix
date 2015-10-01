{ config, pkgs, ... }:

{
  containers.wordpress = {
    privateNetwork = true;
    hostAddress = "192.168.101.1";
    localAddress = "192.168.101.2";

    config = {
      imports = [
        ../../krebs/3modules/iptables.nix
      ];

      krebs.iptables = {
        enable = true;
        tables = {
          filter.INPUT.policy = "DROP";
          filter.FORWARD.policy = "DROP";
          filter.INPUT.rules = [
            { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; precedence = 10001; }
            { predicate = "-p icmp"; target = "ACCEPT"; precedence = 10000; }
            { predicate = "-i lo"; target = "ACCEPT"; precedence = 9999; }
            { predicate = "-p tcp --dport 22"; target = "ACCEPT"; precedence = 9998; }
            { predicate = "-p tcp --dport 80"; target = "ACCEPT"; precedence = 9998; }
          ];
        };
      };

      environment.systemPackages = with pkgs; [
        iptables
      ];

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
      };

      services.httpd = {
        enable = true;
        adminAddr = "root@apanowicz.de";
        extraModules = [
          { name = "php5"; path = "${pkgs.php}/modules/libphp5.so"; }
        ];
        virtualHosts = [
          {
            hostName = "wordpress";
            serverAliases = [ "wordpress" "www.wordpress" ];

            extraSubservices = [
              {
                serviceName = "wordpress";
              }
            ];
          }
        ];
      };
    };
  };
}
