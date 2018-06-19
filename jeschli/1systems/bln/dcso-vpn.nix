with import <stockholm/lib>;
{ ... }:

{

  users.extraUsers = {
    dcsovpn = rec {
      name = "dcsovpn";
      uid = genid "dcsovpn";
      description = "user for running dcso openvpn";
      home = "/home/${name}";
    };
  };

  users.extraGroups.dcsovpn.gid = genid "dcsovpn";

  services.openvpn.servers = {
    dcso = {
      config = ''
        client
        dev tun
        tun-mtu 1356
        mssfix
        proto udp
        float
        remote 217.111.55.41 1194
        nobind
        user dcsovpn
        group dcsovpn
        persist-key
        persist-tun
        ca ${toString <secrets/dcsovpn/ca.pem>}
        cert ${toString <secrets/dcsovpn/cert.pem>}
        key ${toString <secrets/dcsovpn/cert.key>}
        verb 3
        mute 20
        auth-user-pass ${toString <secrets/dcsovpn/login.txt>}
        route-method exe
        route-delay 2
      '';
      updateResolvConf = true;
    };
  };
}
