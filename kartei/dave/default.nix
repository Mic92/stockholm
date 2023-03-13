{ config, ... }: let
  lib = import ../../lib;
in {
  users.dave = {
    mail = "hsngrmpf@gmail.com";
  };
  hosts.dave = {
    owner = config.krebs.users.dave;
    nets.retiolum = {
      aliases = [ "dave.r" ];
      ip6.addr = (lib.krebs.genipv6 "retiolum" "dave" { hostName = "dave"; }).address;
      ip4.addr = "10.243.0.6";
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAoiR04ZkEpM14b9+r260+0+HVnVvd5YESVUiLUzXJkmOjTOeyDwUy
        J/dkX5/Aeu0eIVrv6zkY6FuHoc4BsyObh9SgNWosMms4SE3M5E8xMzep5ahQWLdD
        uCRraDj3XWZzq4YfZntpPGWHHzzbvWKMsPmxAbL/vvCUJLFAPFu8KxIz/TyUUTvq
        vtt7tr9T5p22z9jXgqME5GfQo1hSQgEj+j/k/RGNTX6M0cctewlgD+PC708gVYbt
        f7Yytqazdg6absDC/RPQfD5KRrIxmfYiHekI2IkblRKpd9PTATqdO2XZv+MmD3Fc
        S57fsuoKCCGzhigdmMYuk29naVhKCq3R2wqAWrP0zjTzStCpTjyDvadncqGg6zvS
        o90YNcQPdDfvl61pLE9FAoR6mGc7Dj1H56K7lAlKHr9JObcqIzw/QuYBvkpj+VSP
        9KOHpU9NVRz0xP7FXtOubWwea3NFCQW5mTiukx4wlLk2W3ZFl3Tsvtm19qu5gtHl
        fvuaau/rY6OE9Dx7v9iCkIi+bN3A3tKaufi85nyOtX1B7kq9+mC7kQX8C3iV8QKj
        ceqfMyNp3YlAr5h4dDgxY+GLJmSLhPCtaJ0ZZjjuVuyt1UzxhAbq8LiJ65VqJ1YZ
        xFSrHaDWg7OTpeDjpAjlOTvSE502FMZka1jGGbCjbtzE9iA5b/Dh2o0CAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "VJNzb3ixfyANCWt9pUhHApwVRyS91PUxwmSqS88/53M";
    };
  };
}
