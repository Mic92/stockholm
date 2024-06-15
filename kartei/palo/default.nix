{ config, lib, ... }:
let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };

  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill.ip6.addr =
      (slib.krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
  });

in
{
  hosts = mapAttrs hostDefaults {
    chungus = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          tinc.port = 720;
          aliases = [ "chungus.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAv/6TryKLg/fy14ZRnJht9fQIrzwJW5UikQGjd0Au9ITau1AtoAhO
            6/xgdR4ME8EFckVrmjLoFuZNjVQUOyUJcX6OEX+Fl8veyUn+osgLA9jw4TVpi/DM
            JIb7tNd+OkBbthLu5CiLiBwc+BX1dfSYhUxzxshSIrZ8jln6hUH6hWwi6C0GNfg0
            CBEBjUft31pTGyr/bQUIBsCWDb7+R6UD2wLxXbo1zSAlitNiXoKSA2NvXTbFC4J2
            HpxnhTLlJx8dkwjHn00RGUUrfh8Io/qFdjBEfCmynE+Q6v7a/eJdKxIyAtrzLFBz
            07cG9kM+/H+ldIF1PMqMI/QMbc12HG96/saVfCkTA8Xh+H+9abWfsvrYrQngdahX
            Ubw52yV1lRA+grlzRFmjHyE6iaxlwzJ90Aq5BnnlxpK/P/FKCtd36x1RbhE+rBZO
            VR3ENQRmNowakXdgc+0uS8N5naZIrmO+ficMyBI+Z+CrR0GuDPn++gN833D6KOrs
            NeCHN5C9zi56nDBcS6KLtYWj/Amo5ZEokWqTAB/zCJu+rVACoS7fi3/6pj9fyNaz
            BPEOPKGwROFNq+Bn9jh2jdVmZlO/h+wL8FuxyxaduxWzrZTDo+Pj2iHr1l8cr2VU
            i7zdL3jS4LqE693FHCQE8DufSowa0soYe2+dbalVmKsnt5u9mKuAoj0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "iOJzv56xCoUaqbIe3DfjIqB7In6ZWFbr2DT2jvdLYsM";
        };
      };
    };
    cream = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          tinc.port = 720;
          aliases = [ "cream.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxqrCGJriL5L1ehBf7CrdpL6Ao/ssyj5ZoPdlTP47WtBRahQcp8e0
            xWkAACyiSW2rdvK9hBE4Z7cXHenm8obABl69Q6rLdkxIOM7GBK08cX7ZZrRAdyA1
            Bp9FQWoeHZFq4zBayp889HjPgauglguVlPiXaxh5NhqQkKX4Bkcp4f+OtBMvV0Uf
            kf80J5pknliV/I85VDt0Ofyuuvot9p4GAegeaGaTgIpMrbGvqdpnB+ZiI9lFylCf
            tubRvrX1TsaqrWzFu8B2XL6ZXGCY0IrJXs7P0RsG9OysCK7N9WPVrpX+zGFSCCk+
            3UuKan9AFVOWA72Jj+glIU2i2d3D+Re8kvNmLCQ9GCM2c8Gy+r38UPN1/WTEe7az
            94ivkczOgg4tIzMCN2JuAYLtoy3JK46Bbexk3i9KgtX5acNrKilQBDKHktqr0oJ8
            Bz53kFP/X7oY+0RIPePL9OPQu8LRyFXeWeuQQUBgqDmttoWBtHEO6vicKFgwN0bl
            5J6urUJQYC7aabfYO4aDfgVSRr7cELZkbIsx6Lkj5bOrraaJ2pS5H3QGSBUFifAq
            mUdKKkBsYltKe8BsqKvQEysT3cGaGlkeP5OaKHN4qG7hGvLk71YjrYlWlIswdMAp
            D2UgJ5/fcDswSAnFBlLYIqQwC7vMLoqTZPkQ0AN/DxHJCuXfRoU2vhkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "lkvs1E4lCXt+Q7lvg/vU2JQyDfqseYo68Ecbb/Hg8YA";
        };
      };
    };
    cherry = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          tinc.port = 720;
          aliases = [ "cherry.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuDU2RZX+Hwa0sI/ZjnPd/gBLFs/KsS8SE18GEB8X/EH5D11qvRGi
            myAYbHdx5hWbFuJ6GyUL/k99y0vV3VI+IGLyZ3FmVUdPF5fK495+qas68GzBMwhw
            FilessbI1UvBjUITIpVibDW4jSt5ex5SeGSm7ZGpamVFNe442SYgL48V5B3nDRrf
            jdfwxXuBW5iYi7wxuUncfVkFHrh/HNhOQhqHDhWIlgckymsGMszz0sPNfZZOVw8g
            7OA0c4Pduc140icyTaNQzNQZ4KipeZpjUXWn2oGP9ZIb3AiB07BZv3b2x6NrHucW
            vWKtRkpBOMtYrIhVSuGE3MYHSCxrCR+e4moeeTUjpI5hPe9qtRwagKgchq3sxwWq
            p1FfT3zEjZKT2tYRu3w/DWgOize+HvF8GDFTNAcp6APUsyE3mTJkpgsAfG8Lkq+f
            RbrtEOqqqwKCz7IgJDnRSVHlaLP8v3LarhTVIqCkgveyd7wd9bh8fvpWc8fdnWXw
            +WnIOj+KFxLIGr2xReRPbfi9SRnwTtt0E+oXEPC2tUrcWXGNMeiBmVUGH66pRnFP
            wzMBn1q4GzqlG2TVsbXL6Nbzw8caDKH3MGPUUxikJXUODk7PtnQ9TThlsg8xdMGr
            JCrPwL32VNXwXhxJ9vgxaXIfxIanye45j8lxwdnMkx5laULGWVq53bECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "QvtcXLmAviX/uv0FPrFak1WV+U2WwyO5dGciRlPB2lG";
        };
      };
    };
  };
  users = {
    palo = {
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC6uza62+Go9sBFs3XZE2OkugBv9PJ7Yv8ebCskE5WYPcahMZIKkQw+zkGI8EGzOPJhQEv2xk+XBf2VOzj0Fto4nh8X5+Llb1nM+YxQPk1SVlwbNAlhh24L1w2vKtBtMy277MF4EP+caGceYP6gki5+DzlPUSdFSAEFFWgN1WPkiyUii15Xi3QuCMR8F18dbwVUYbT11vwNhdiAXWphrQG+yPguALBGR+21JM6fffOln3BhoDUp2poVc5Qe2EBuUbRUV3/fOU4HwWVKZ7KCFvLZBSVFutXCj5HuNWJ5T3RuuxJSmY5lYuFZx9gD+n+DAEJt30iXWcaJlmUqQB5awcB1S2d9pJ141V4vjiCMKUJHIdspFrI23rFNYD9k2ZXDA8VOnQE33BzmgF9xOVh6qr4G0oEpsNqJoKybVTUeSyl4+ifzdQANouvySgLJV/pcqaxX1srSDIUlcM2vDMWAs3ryCa0aAlmAVZIHgRhh6wa+IXW8gIYt+5biPWUuihJ4zGBEwkyVXXf2xsecMWCAGPWPDL0/fBfY9krNfC5M2sqxey2ShFIq+R/wMdaI7yVjUCF2QIUNiIdFbJL6bDrDyHnEXJJN+rAo23jUoTZZRv7Jq3DB/A5H7a73VCcblZyUmwMSlpg3wos7pdw5Ctta3zQPoxoAKGS1uZ+yTeZbPMmdbw==";
    };
  };
}

