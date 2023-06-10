{ config, lib, ... }:
let
  slib = import ../../lib/pure.nix { inherit lib; };
in
{
  users.ynnel = {
    mail = "retiolum@lenny.ninja";
  };
  hosts.mokemoke = {
    owner = config.krebs.users.ynnel;
    nets.retiolum = {
      aliases = [ "mokemoke.ynnel.r" ];
      ip6.addr = (slib.krebs.genipv6 "retiolum" "ynnel" { hostName = "mokemoke"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEA7rS560SZEPcSekW30dRF6ZTHOnb8WvuVgt3BFLRWhTgV5DqLqFa8
        fxT2TJci8ogYZtlnSCNKEhxup3wlIrAPLLzu5jL6hx4okfmyARGQqeUn9kD+jmGL
        9N9wjGXDp/CVyMIb5mcK2l0mvElvs7ae700GScq+2ASsFTHC/w2w2KoeDtt/UED9
        Cjy+kxP7SuzksigIuuA8gncf9FmfRgG31XGctX1H6hUywtq05oVRd5qMHeiI/l4v
        jHJSadtlR1FuExMT9l7nRZ98yOLKWhDUym4qmi/3zsnDl38f9gcqlp040McUqfZl
        6mclphcthOv6xp7nCbEd58djBU1hrPHJJrk5qL0CGcTwaTBzZFvrV4lklfBFPhVv
        dwiagzZDsTvQfXe7UJTSHOKhw+i7a7ok2n+IFhyd+GnQYeOvaBropjYgYDHbZ/u7
        d6E1xUVjANLtt2oOYfaH/LlERgucEcQY2qRyMBQXYTwp+d3ThTc+Vs0Lbo08rvFN
        y76KXPsH8ptVVFK4DclK0GxI64JpnSmG/BHcU114K7LPNONQBSvE8UyZlMVkuZfc
        qwBzyM70tKPoWmoxjBkQcXsK6JgclXohZ0jbMhRV5K4oDocAhEuUtOC5qG4IZo+R
        BWc0bxueCaOQFqB6UKcZLgCj6ZhXHpqTSk/8MBevxrbH44I+4oYwQOkCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "um4yKCJkkBX9pISAa78SttNSqyEPhpCDGfL6FJA0wzK";
    };
  };
}
