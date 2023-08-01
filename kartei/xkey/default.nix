{ config, lib, ... }:
let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  maybeEmpty = attrset: key: if (attrset?key) then attrset.${key} else [];
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
    owner = config.krebs.users.xkey;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum = {
      ip6.addr = (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
    };
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill = {
      ip6.addr = (slib.krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
    };
  });
  ssh-for = name: builtins.readFile (./ssh + "/${name}.pub");
in
{
  users = rec {
    xkey = {
      mail = "xkey@krebsco.de";
      pubkey = ssh-for "xkey";
    };
  };
  hosts = mapAttrs hostDefaults {
    aland = {
      nets.wiregrill = {
        ip4.addr = "10.244.12.34";
        aliases = [ "aland.xkey.w" ];
        wireguard.pubkey = "m2IymGYQiRma2cyZbwRsOw1rCpB5ZdFkfYII1hnHzGE=";
      };
    };
    catalonia = {
      nets = {
       retiolum = {
         ip4.addr = "10.243.13.12";
         aliases = [ "catalonia.xkey.r" ];
         tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEAug+nej8/spuRHdzcfBYAuzUVoiq4YufmJqXSshvgf4aqjeVEt91Y
           gT6iBN8IKnMjYk3bAS7MxmgiyVE17MQlaQi0RSYY47M8I9TvCYtWX/FcXuP9e6CA
           VcalDUNpy2qNB+yEE8gMa8vDA3smKk/iK47jTtpWoPtvejLK/SCi8RdlYjKlOErE
           Yl9mCniGD1WEYgdrjf6Nl7av6uuGYNibivIMkB2JyGwGGmzvP+oBFi2Cwarw8K2e
           FK2VGrAfkgiP5rTPACHseoeCsJtRLozgzYzmS5M9XhP5ZoPkbtR/pL5btCwoCTlZ
           HotmLVg4DezbPjNOBB9gtJF4UuzQjSPNY6K1VvvLOhDwXdyln82LuNcm9l+cy9y3
           mGeSvqOouBugDqie6OpkF0KrRwlGQVwzwtnDohGd/5f7TbiPf1QjC+JP/m4mxZl3
           zE0BCOct9b4hUc/CFto71CPlytSbTsMhfJAn8JxttGvsWIAj+dQ0iuLXfLDflWt6
           sImmnOo28YInvFx6pKoxTwcV1AVrPWn5TSePhZM50dmzs0exltOISFECDhpPabU3
           ZymRCze8fH9Z3SHxfxTlTZV7IaW2kpyyBe1KsWpM46gLPk5icX+Xc6mdGwbdGBpf
           vDZ+BoHCjq9FfQrAu1+E83yCYyu+3fWrLSgYyrqjg0gPcCcnb1g6hqECAwEAAQ==
           -----END RSA PUBLIC KEY-----
         '';
         tinc.pubkey_ed25519 = "PiqJGofbo6941m20NJM3yhUoWKTNyLCtTPzsKcrvFSL";
        };
      };
    };
    cybercube = {
      nets.wiregrill = {
        aliases = [ "cybercube.xkey.w" ];
        wireguard.pubkey = "ZPOCyThKQUlR/gPFWoJ4XICHYFMNtI70XH+y5v2f6VQ=";
      };
    };
    rojava = {
     nets = {
       retiolum = {
         ip4.addr = "10.243.23.42";
         aliases = [ "rojava.xkey.r" ];
         tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEA3Xafx5PYDNRxRwWGo25paveBgEFQYWWOg5YYcqSlBsUzWkEwZPdd
           B0O8xJDIS3SDZrDW5aC43RGe+l6L68OBzB79DNAhxcdzzDQkAqI4IsaWBzgEFIbb
           HG+Asx2ZN1biykCR4GN77JYGwa7RrCgsA3LdT6ICGPWbLU3M/QeaIbTooDq/PF61
           Eu8i/S/qqhC/KBDq9CXL+amiyjoe4l+iLIKtCmvJZge1v8cc9n4iHqfP1JPXMPrD
           lu9Mshxy8um62oaC/jvyw9R511LaEcT/Hvxi030tiL/H/1dOIhx+4RJsapHGw4LW
           +ud1UAU8WXSRmYqeRw11+obZycnxZF0R0xEKGVIxCnf+vAriEM2iqruRKP1gYVzs
           3DW+dq5eirkzdmJZsTY3lX+q/hR9lfzQFuq9G6lrqKyx5L7FZNCMviMfw63TfHF2
           vV4D77hrRH1yp/c5UUo8H9j9/u62JyZ/pSszjKgVy+nSD+zJ6waEZWip7T8V/pmx
           HOTIZC5xGKyxX/6DTVU7YJzLlaiZLJ3RudNrTXY9w24NEhum5A7BaEmyJbbqRdx+
           XJ3+vf9jPCW9wUGKO5vsu67x/xy8eEVx7Tm5aVWlpXGvlfTiOvhUCPNDOa/HMYp4
           yuy4xLEIhAlt7jI02aYe3Cj3CbJEYdNJj+qBPzpfKCuCyATQzGmgaq0CAwEAAQ==
           -----END RSA PUBLIC KEY-----
         '';
         tinc.pubkey_ed25519 = "WuvA0epfMZnPysLc+oKQydgWAz9/Mc+fM1DujeKj65F";
        };
      };
    };
    sicily = {
     nets = {
       retiolum = {
         ip4.addr = "10.243.161.1";
         aliases = [ "sicily.xkey.r" "mukke.r" "bie.r" ];
         tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEAzjCrsMRptg22QJTXsNgrxE/CjpGiDD9NYExqiDQ7kyKJ7+nrjtJg
           aI1bL7CmlfbleE47VmkZBbyglI7wELA0X//WW6laz/5XwBKQyYSgt1ZtcordYoam
           xeNmV9a4dcpYO5y+YXxac8epX8TVSu1c0H7jEMcGrvTXDZwijEPQTMCvj2pookod
           1seiLKjKZTW7TWVUZ3Hi/NZh2EEZu/mN0zZbGSGQv0cDdD6/kxw/ZstE6c7cYF7/
           IFdGLuLGa60em8AKCFT0WXRF9UnuZ7txw96qcrZotIlSY9ssJf8veBFDfiyKWiO7
           KBZXa7c2/5T+GOIBr/XZGH6vpCMFIuHq8A7wWPcbV0NvA6yssn8R7LrrEC2qU+RC
           7DhUwC70tODQyZ4IT/8eEntGdJwi4Zy6Uer5EMFkHCTBG6N3xKev+LppH+HGwH9L
           LJ1qGEhK7PFcXFyLMEnBu4f316BEf9Hii4xDegBICTHGQfsHI2xE1GfeToqkvnyp
           T4BgR6f6wVPsj+nP7UkCacIOtgUyjcTVuf4Da8PsX0liEYOcxSl2t9uZ1ks82DQB
           w+p3Y03KRQh8TpidHWyydkya25xCO8x0t6q1q2xlIVKClGb3EG8YFRM+nEKT5sZO
           8nhqW50G+zUK3Y4vI3qzKjG9T5xi8Jwy8Zqd2h0VkNWXpn3NqqZkZwkCAwEAAQ==
           -----END RSA PUBLIC KEY-----
         '';
         tinc.pubkey_ed25519 = "G7t9IdhukaYPMc82H/EqEiH10t5C4DneQpcxJDiUjqN";
        };
      };
    };
  };
}
