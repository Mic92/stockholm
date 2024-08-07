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
            MIICCgKCAgEAnzhalF1rqLdSsT6HAGuQ6x1kC9Ty3FjoKR2Y5RCO9YIyEgRE8qfR
            jkne+wIIleODUDMZYuvUe9X5hm8w6wDzxlwCPitwhDlOxoSBnXfbL6YL9rZBn3lC
            JFkpEPtAJYnfM64R4/UjSndHlCVuH7tltD/1tmfG6IbSsIeDVz+pWZdEmBJfCiDl
            aqP2gb1oIwe9TgJX2EC2ugW+6Jh9oPNIOP2Q5eLvty5WPhUSGQDWVMr5u0Rgc1oU
            hhAvrfue7MFqUwX+o0Zq93eVAu/51dhTtqwwVgZVlHK7Wkak4yTRGPAP9v9vbKeK
            7GpQuvbiI5OphhSFPjyCN1XMqVgFxqsnLsflIPbQdxCkBgFxhmNf31BDlXWHWD5e
            7BfFYc1tZFcEWKhguoCSesJvh1BVsiZzfya96lGd/+ttcKBUKX4tdznEQsV/MVhC
            cVnQD6k8PN4BIWVJtcq5oM9h6Yt6avtv8TeuaLp/Janco4JmYYFIfRETnz6ye/fG
            OiKJnGQ1yohSE6n8ZUK1QYdYezZfI8QhF7GHK7he9x13L9xmXoybV+REXlRvh4S2
            bi9lWTKhQVIHb/qLIdQuaAnK1xg4tdNzL43KEpPstGlAnG8uUNL8hCJL3m220RPK
            lEbtLhayRzQ9zgj/hBQZa/hMGGyiqV1hiTbEEWAusJdGTUPYhjAelOkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "B3EKYRxqFjIGR2VYajjDqX0gltPJNwcno5PUhafKWKB";
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

