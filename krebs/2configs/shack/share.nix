{config, ... }:{
  users.users.smbguest = {
    name = "smbguest";
    uid = config.ids.uids.smbguest; #effectively systemUser
    group = "share";
    description = "smb guest user";
    home = "/home/share";
    createHome = true;
  };
  users.groups.share = {};

  networking.firewall.allowedTCPPorts = [
    139 445 # samba
  ];

  networking.firewall.allowedUDPPorts = [
    137 138
  ];
  services.samba = {
    enable = true;
    shares = {
      share-home = {
        path = "/home/share/";
        "read only" = "no";
        browseable = "yes";
        "guest ok" = "yes";
      };
    };
    extraConfig = ''
      guest account = smbguest
      map to guest = bad user
      # disable printing
      load printers = no
      printing = bsd
      printcap name = /dev/null
      disable spoolss = yes

      # for legacy systems
      client min protocol = NT1
      server min protocol = NT1
      workgroup = WORKGROUP
      server string = ${config.networking.hostName}
      netbios name = ${config.networking.hostName}
    '';
  };
}
