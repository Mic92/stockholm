with import ../../lib;
{ config, ... }: let

  evalHost = hostName: hostConfig: evalSubmodule types.host [
    hostConfig
    {
      name = hostName;
      owner = config.krebs.users.tv;
    }
    (optionalAttrs (hasAttrByPath ["nets" "retiolum"] hostConfig) {
      nets.retiolum = {
        ip6.addr =
          (krebs.genipv6 "retiolum" "tv" { inherit hostName; }).address;
      };
    })
    (let
      pubkey-path = ./wiregrill + "/${hostName}.pub";
    in optionalAttrs (pathExists pubkey-path) {
      nets.wiregrill = {
        aliases = [
          "${hostName}.w"
        ];
        ip6.addr =
          (krebs.genipv6 "wiregrill" "tv" { inherit hostName; }).address;
        wireguard.pubkey = readFile pubkey-path;
      };
    })
    (host: mkIf (host.config.ssh.pubkey != null) {
      ssh.privkey = mapAttrs (const mkDefault) {
        path = config.krebs.secret.file "ssh.id_${host.config.ssh.privkey.type}";
        type = head (toList (match "ssh-([^ ]+) .*" host.config.ssh.pubkey));
      };
    })
  ];

  hostFiles =
    mapAttrs'
      (name: type: {
        name = removeSuffix ".nix" name;
        value = ./hosts + "/${name}";
      })
      (readDir ./hosts);

in {
  dns.providers = {
    "viljetic.de" = "regfish";
  };
  hosts =
    mapAttrs
      (hostName: hostFile: let
        hostSource = import hostFile;
        hostConfig = getAttr (typeOf hostSource) {
          lambda = hostSource { inherit config lib; };
          set = hostSource;
        };
      in
        evalHost hostName hostConfig)
      hostFiles;
  sitemap = {
    "http://cgit.krebsco.de" = {
      desc = "Git repositories";
    };
    "http://krebs.ni.r" = {
      desc = "krebs-pages mirror";
    };
  };
  users = {
    dv = {
      mail = "dv@alnus.r";
    };
    itak = {
    };
    mv-ni = {
      mail = "mv@ni.r";
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGer9e2+Lew7vnisgBbsFNECEIkpNJgEaqQqgb9inWkQ mv@vod";
    };
    tv = {
      mail = "tv@nomic.r";
      pgp.pubkeys.default = readFile ./pgp/CBF89B0B.asc;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDFR//RnCvEZAt0F6ExDsatKZ/DDdifanuSL360mqOhaFieKI34RoOwfQT9T+Ga52Vh5V2La6esvlph686EdgzeKLvDoxEwFM9ZYFBcMrNzu4bMTlgE7YUYw5JiORyXNfznBGnme6qpuvx9ibYhUyiZo99kM8ys5YrUHrP2JXQJMezDFZHxT4GFMOuSdh/1daGoKKD6hYL/jEHX8CI4E3BSmKK6ygYr1fVX0K0Tv77lIi5mLXucjR7CytWYWYnhM6DC3Hxpv2zRkPgf3k0x/Y1hrw3V/r0Me5h90pd2C8pFaWA2ZoUT/fmyVqvx1tZPYToU/O2dMItY0zgx2kR0yD+6g7Aahz3R+KlXkV8k5c8bbTbfGnZWDR1ZlbLRM9Yt5vosfwapUD90MmVkpmR3wUkO2sUKi80QfC7b4KvSDXQ+MImbGxMaU5Bnsq1PqLN95q+uat3nlAVBAELkcx51FlE9CaIS65y4J7FEDg8BE5JeuCNshh62VSYRXVSFt8bk3f/TFGgzC8OIo14BhVmiRQQ503Z1sROyf5xLX2a/EJavMm1i2Bs2TH6ROKY9z5Pz8hT5US0r381V8oG7TZyLF9HTtoy3wCYsgWA5EmLanjAsVU2YEeAA0rxzdtYP8Y2okFiJ6u+M4HQZ3Wg3peSodyp3vxdYce2vk4EKeqEFuuS82850DYb7Et7fmp+wQQUT8Q/bMO0DreWjHoMM5lE4LJ4ME6AxksmMiFtfo/4Fe2q9D+LAqZ+ANOcv9M+8Rn6ngiYmuRNd0l/a02q1PEvO6vTfXgcl4f7Z1IULHPEaDNZHCJS1K5RXYFqYQ6OHsTmOm7hnwaRAS97+VFMo1i5uvTx9nYaAcY7yzq3Ckfb67dMBKApGOpJpkvPgfrP7bgBO5rOZXM1opXqVPb09nljAhhAhyCTh1e/8+mJrBo0cLQ/LupQzVxGDgm3awSMPxsZAN45PSWz76zzxdDa1MMo51do+VJHfs7Wl0NcXAQrniOBYL9Wqt0qNkn1gY5smkkISGeQ/vxNap4MmzeZE7b5fpOy+2fpcRVQLpc4nooQzJvSVTFz+25lgZ6iHf45K87gQFMIAri1Pf/EDDpL87az+bRWvWi+BA2kMe1kf+Ay1LyMz8r+g51H0ma0bNFh6+fbWMfUiD9JCepIObclnUJ4NlWfcgHxTf17d/4tl6z4DTcLpCCk8Da77JouSHgvtcRbRlFV1OfhWZLXUsrlfpaQTiItv6TGIr3k7+7b66o3Qw/GQVs5GmYifaIZIz8n8my4XjkaMBd0SZfBzzvFjHMq6YUP9+SbjvReqofuoO+5tW1wTYZXitFFBfwuHlXm6w77K5QDBW6olT7pat41/F5eGxLcz tv@wu";
      uid = 1337; # TODO use default and document what has to be done (for vv)
    };
    tv-nomic = {
      inherit (config.krebs.users.tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3dYR/n4Yw8OsYmfR2rSUG7o10G6AqOlSJHuHSEmANbMkqjiWl1TnpORtt5bAktyAGI4vWf9vhNEnxuIqGXWSV+3yCd7yfBHR1m0Y9QSw6blQ0xc1venl3JU0kpEyJfUn8a9cdXlnRiS0MP1gcsN7Zk8cqBELJYJajkSEnsT4eVaU5/wdnyzUO1fk8D8tFBJbF/tsWDLJPu4P18rpxq4wZgA2qmyHoVDEVlrz2OYcziXT6gpG0JGnToteaNg9ok5QavEYFpp8P+k1AacrBjc1PAb4MaMX1nfkSyaZwSqLdH35XkNRgPhVVmqZ5PlG3VeNpPSwpdcKi8P3zH1xG9g6Usx1SAyvcoAyGHdOwmFuA2tc1HgYEiQ+OsPrHZHujBOOZsKTN9+IZHScCAe+UmUcK413WEZKPs8PeFjf1gQAoDXb55JpksxLAnC/SQOl4FhkctIAXxr12ALlyt9UFPzIoj/Nj2MpFzGSlf653fTFmnMbQ8+GICc4TUpqx5GELZhfQuprBTv/55a9zKvM4B8XT3Bn9olQzMQIXEjXb3WUVFDDNWeNydToorYn1wG3ZWQ+3f0IlqRicWO7Q9BRj1Lp5rcUCb+naJ48tGY6HFUZ1Kz/0x458GDFvUd8mCJjqqmeSkUEeZd0xet5tVFg/bYoSslEqPF6pz7V3ruJMSdYxnQ== tv@nomic #2";
    };
    tv-xu = {
      inherit (config.krebs.users.tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC/3nkqxe8YrDVt615n96A7iC3vvwsiqgpsBYC/bhwfBHu1bAtBmTWVqSKDIdwg7p8TQpIKtAgZ3IJT3BlrnVTeR4RIviLjHjYWW1NBhm+nXi+heThgi5fLciE3lVLVsy5X9Kc1ZPLgLa1In0REOanwbueOD0ESN1yKIDwUUdczw/o3dLDMzanqFHKuSSN4o9Ex2x+MRj9eLsb706s4VSYMo3lirRCJeAOGv1C7Xg1cuepdhIeJsq9aF7vSy15c0nCkWwr8zdY7pbMPYCe5zvIEymZ0UowZ5HQ3NmIZnYDxa4E1PFjDczHdQbVmmGMI80grNwMsHzQ6bynHSPXDoLf4WodXlhS0+9Ju5QavDT6uqZ9uhDBuWC8QNgWUMIJnEaTBFyA0OI1akl8Q2RLC+qnNf5IwItSq+GDwEsB2ZJNW3kOk1kNiCUrBafRYpPaFeP97wzzP4uYlBKAr2SOLrrkf7NFEdw2ihxhDMNnps/ErRJ8U0zdpmalw8mItGyqRULpHjk/wN00rYOdBIhW3G3QJuVgtGnWtGCBG5x70EfMiSEXPD3YSsVVsgKD+v8qr+YiilRRD+N3gaHhiOWA6HgxRNul/P4llk0ktTpb9LoHk2+oooTH5ZuuT/8yF8J4stZt7EIOH+mSOAXG1z0BwnEkQu7pVKwu/oOZpGJTvBrGwww== tv@xu";
    };
    vv = {
      mail = "vv@mu.r";
      uid = 2000; # TODO use default
    };
  };
}
