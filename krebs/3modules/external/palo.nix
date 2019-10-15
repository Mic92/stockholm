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
      syncthing.id = "FLY7DHI-TJLEQBJ-JZNC4YV-NBX53Z2-ZBRWADL-BKSFXYZ-L4FMDVH-MOSEVAQ";
    };
    sterni = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.3";
          tinc.port = 720;
          aliases = [
            "sterni.r"
          ];
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
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDBYNJVuyyZmc2pCkLWjhl0/hMMb7elmI81/9LAGtk8Tz4TmVderTMohwQkaTYznwPOPuKfU1sSMLCB8rYXdAO5nqWC4bGjXJ/+D8/UKfGjSqRQ7UkfpOF3NAm+pqUSFjaVXi1BWd+jxmsD0uRks0PyNSywZfgjn5LYpD3SpxyFy/17P/PJ9vX6PELjeYvNGH3l5cXDwYky3ZZJol7quBJ5yrA6I536A4wNDzg2ow+MRVu51/nIJdnbbsC/dDHgmdRWnStOzvsA+xSEMeKvLW3CaSPINr/bMGxOPrefr79bg59gkw9Wxp51fkx0o18N1liTRfWXau+GFNGMxFluELhfGXYOH9HLedLt8H38zs5vgJ9IY+tlOzMKud5njiNkuG503AiqY2H7coN7VeVA5+6L7tmwFbCMhPal4MS0VKHNBmCTDY5QMURYUajKiUh8n5IcbuTsPM+lEszm16g5iB+XQ1vpjza5ds6DRL1H6pUF/UpUzYUlqh2RnE+CyLsFO2MB/o72NoSWRfmn7/nsg6eEg/9kSn+dwj2ythjuEkMG28Yhm/XjaGnuAE/ZpIeRDozIQNGcHpzPHMd95olfNJW7+fLi+CvSFZa9l+tdS8PoRnCdHOsO4zvESJZ2rDn0Zt0Az6XNRJfYTABDlYPGCnWN4vmlnEJqQARSSiKBDhSgPw== palo@workout";
    };
  };
}

