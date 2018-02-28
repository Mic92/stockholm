    {
      services.taskserver = {
        enable = true;
        fqdn = "enklave.r";
        listenHost = "::";
        listenPort = 53589;
        organisations.lass.users = [ "jeschli" ];
      };
      networking.firewall.allowedTCPPorts = [ 53589 ];
    }
