with import <stockholm/lib>;
{ config, ... }: let

  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill.ip6.addr =
      (krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
  });
  ssh-for = name: builtins.readFile (./ssh + "/${name}.pub");
  tinc-for = name: builtins.readFile (./tinc + "/${name}.pub");

in {
  hosts = mapAttrs hostDefaults {
    pepe = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.1";
          tinc.port = 720;
          aliases = [ "pepe.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    kruck = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.3";
          tinc.port = 720;
          aliases = [ "kruck.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    schasch = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.2";
          tinc.port = 720;
          aliases = [ "schasch.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    workhorse = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.5";
          tinc.port = 720;
          aliases = [ "workhorse.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    workout = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.4";
          tinc.port = 720;
          aliases = [ "workout.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
  };
  users = {
    palo = {
    };
  };
}

