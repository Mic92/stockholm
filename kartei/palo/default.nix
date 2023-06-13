with import ../../lib;
{ config, ... }:
let

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

in
{
  hosts = mapAttrs hostDefaults {
    sol = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          tinc.port = 720;
          aliases = [ "sol.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxrvdMSAcOJXM1TbIIDZ+zPojrcRG3RVMfPC2/0DasRpBFSuS+L60
            mQEs0l0ptAL6Sbr4+9gfaHkdETfYpeKB4Q4lCPahMq88YfTyB1f3tEOqW3vP22nC
            Z+Yf+W/sTLWVRoDoS/Eok6wS95R1IQ74vr37YXdbJTD/eeX6sAJkn2I2RV5PD6Bu
            lHsMuunAj+PyhAgqb2P393h7FN4exL0xM6UbHbgsd9OSp5qKTjZE3jeOyWmounK1
            7n+8pyRjI0VE47ontnj/GANwpsxRFFtRGmG/S5KhUBXMv7wZr/vaVETRphAu+KhT
            NqdclmGkQlB/YBodzJID7C21Zz4b33kcn12TU3nc6AL5u9j3sU2sEu/22fAZBWLV
            yOZ9l/Qe4aJkIbdL70Gvp9G8m7+M4vkdM+e/nA5cZT0N9ArI2D5ltJRd7VLVzxef
            Y0t/bS9bVOcNt2Sgd81Ubg0OmF2paHGGboAAMqXhf3afwCMyXcDsP6sgPXOIEu7Q
            hjuo5rg6Fu8eK9edAAQ2afl52GiFUawzjHbjGANwVyea1JTQ3uR6eBtxGOEaYpkr
            vbl75CxLwE0YA0L3VwhJTNLMVldTrUi2M76QedjzyePkJHMijHT5+0nqTlsmjcNg
            uv89Mh9shNKdqulfGjTAFyKjTCuUe/rCprJ5CeZWBaEuQKYkcZuMkJsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "qCJvjlNz5YNOz5IEiwGaoK3InSVCL76uNl+xVBUa/AP";
        };
      };
    };
    centauri = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          tinc.port = 720;
          aliases = [ "centauri.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxrvdMSAcOJXM1TbIIDZ+zPojrcRG3RVMfPC2/0DasRpBFSuS+L60
            mQEs0l0ptAL6Sbr4+9gfaHkdETfYpeKB4Q4lCPahMq88YfTyB1f3tEOqW3vP22nC
            Z+Yf+W/sTLWVRoDoS/Eok6wS95R1IQ74vr37YXdbJTD/eeX6sAJkn2I2RV5PD6Bu
            lHsMuunAj+PyhAgqb2P393h7FN4exL0xM6UbHbgsd9OSp5qKTjZE3jeOyWmounK1
            7n+8pyRjI0VE47ontnj/GANwpsxRFFtRGmG/S5KhUBXMv7wZr/vaVETRphAu+KhT
            NqdclmGkQlB/YBodzJID7C21Zz4b33kcn12TU3nc6AL5u9j3sU2sEu/22fAZBWLV
            yOZ9l/Qe4aJkIbdL70Gvp9G8m7+M4vkdM+e/nA5cZT0N9ArI2D5ltJRd7VLVzxef
            Y0t/bS9bVOcNt2Sgd81Ubg0OmF2paHGGboAAMqXhf3afwCMyXcDsP6sgPXOIEu7Q
            hjuo5rg6Fu8eK9edAAQ2afl52GiFUawzjHbjGANwVyea1JTQ3uR6eBtxGOEaYpkr
            vbl75CxLwE0YA0L3VwhJTNLMVldTrUi2M76QedjzyePkJHMijHT5+0nqTlsmjcNg
            uv89Mh9shNKdqulfGjTAFyKjTCuUe/rCprJ5CeZWBaEuQKYkcZuMkJsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "qCJvjlNz5YNOz5IEiwGaoK3InSVCL76uNl+xVBUa/AP";
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

