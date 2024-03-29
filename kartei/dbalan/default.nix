{ config, lib, ... }:
let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  hostDefaults = hostName: host:
    flip recursiveUpdate host ({
      ci = false;
      external = true;
      monitoring = false;
      owner = config.krebs.users.dbalan;
    } // optionalAttrs (host.nets ? retiolum) {
      nets.retiolum = {
        ip6.addr = (slib.krebs.genipv6 "retiolum" "external" {
          inherit hostName;
        }).address;
      };
    } // optionalAttrs (host.nets ? wiregrill) {
      nets.wiregrill = {
        ip6.addr = (slib.krebs.genipv6 "wiregrill" "external" {
          inherit hostName;
        }).address;
      };
    });
in {
  users = rec {
    dbalan = {
      mail = "dbalan@thaum.space";
      pubkey =
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAiWF+U3VHNfp1IPU0/TWhMioxJvmoyG1AMZMvnQjy5QAAAABHNzaDo= dj@v60";
    };
  };
  hosts = mapAttrs hostDefaults {
    tofu = {
      nets.retiolum = {
        aliases = [ "tofu.dbalan.r" ];
        ip4.addr = "10.243.43.12";
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAlBMwSkdI+3CWPVazfDoPCoqYJH1MLd8Z/ZQCTFx4MhLO/DvJrb7H
          HJEwvFuImk2yy4oYfcRz8pK+ymBDArdVT5aeTwztTbjkcWa5RTm2d2xuYppTUPZB
          L0/yY+a+mP1eRa8O6HAoNx3ETvC5euJS40qISTh6grPGFV7i1HhGNPUkvYFlNF6q
          rJs7RgC8Y2/OSNed5WdJfNYNBeB4xUiKtg3sShKQGO++860yNV4G+J+PtBVnMs8/
          MnxQbJxnMbhAHNQt5nV+Z66Ewy/2qcn4clrGB3SG1v9fFizzh9mAFK3Udfa+6s8r
          JaM4BL41uOgHxiiKovmudxIRtts31cKzwPkMyThQpK8hdxJxrCzSQSbiDUkckE+5
          CeLgAh5jMwEkT6OvYOGZbyeqvZX5gX+yErJyRlsjgYs5TTlfUnpg1BKz2BErzb1b
          +lWsZXjO03BpNXsuWhC2sspKfuB3+fjlNwR+ov8/UDTOwhzBz15TlufcxiqzA08d
          yJiNylm07C19qxlffqSZ5KSZGTT+DZg53ror1PLMoxLApV5MgHiM6oltDxemSegb
          yySGz/EFDDbQQYLYOPu6seLt+ktudvFTERmetV0Bn37Ok8Avs2y6BIgXWjVxcb5p
          EDbE5tAvX/MZIU9rAXRqDAOulumfvsyFpK8RkXDEoIQEt+gDoZ5OzJsCAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "b355fDYFQgFFj3KfzUCv4DPSfoFTKklsB12imwci7fH";
      };
    };

    v60 = {
      nets.retiolum = {
        aliases = [ "v60.dbalan.r" ];
        ip4.addr = "10.243.42.12";
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAxVRxcCWfjLu9cNo5ELfXyuwhpJBSfod5f9JkclSpydVHaQBfeVC6
          RKfdknQVL6RXiCMFsSAvCvmnIohmpUCbiQWu29P/g0jzQZZ7zNx5L7JHy18x9qAr
          1scu7FRdVErVuWKXXNt0+j45dA+u5HE6RLsjAHGYtQbAr21VLyLF3qq11IWNrFYU
          uqSnM/ZPbOPPHLS8XtsQRdJ2cOkccSCO4W6xBar92aPFuDImH60VuxMFEKYWY2bz
          p6q0K0rtRqW1qANTV62SUDeA1wMPlSmvnMFY7qesSLk6tJjJ02HwwiOvK2ov1/Rm
          bpwcrqrrbUxbCaZC6t7pBBxUOZlGfnO3woZQm63+4TEw/YDHhxD0HbhH88Wc+eHy
          I73tuL1oc01JxL131bJV6jcHG7LrG7wTsTdDaZpjbH54adJP47QpTMb0ggsx2WkD
          mpxFFSnTZL7ghZO5NGPvidTBp+wJiSOv5igAjA72CvjR3tOF4d5Lsq4JsQeCStjA
          OPrIrN0AnJRg2IFDXZEGwTS9AbLWX147O9VrNimLzezOylH4Eihn7GUJ5KLIPjLy
          AvsgIYljoJuhGbM8QoWlakwqOndMeoqhz52ORZ5CDgfybJJEbyrYF8gYFVNJOzds
          9gy/F+27TwfjMgcheN2+ogJp+lD754aCF0EJMwaK8ElzQLqAzbBRGAsCAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "dcPFpCG94cq1KHD4TH9WgOl9fpc1589YvWkmnkEZcSC";
      };
    };
  };
}
