with import <stockholm/lib>;
{ config, ... }: let

  hostDefaults = hostName: host: flip recursiveUpdate host {
    ci = true;
    monitoring = true;
    owner = config.krebs.users.mb;
  };

in {
  hosts = mapAttrs hostDefaults {
    orange = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.42.23";
          aliases = [
            "orange.r"
            "or4ng3.r"
            "0r4n93.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAr7P0CkmC5HWnTdgGFzmA
            zQuJzHSkSjcGgSkIt0pvqU6xi8P/d4eJlmeXeGTpH62JfM1xhEMpxMVd/4NOON2u
            IlWnfu5bB763145IJwE0HmZziWjQXWRPAZMqYdQ5f2Pvmxv1yr3uBNzr8UlV6BjD
            FXn8sCvikXttYzts9szlz5+pkY09qfiz48+DMzRBNO6JzXYQ9kPyS+TIXlGpN4Jp
            C1TRF38eF2DTEZ58Yx8Z99dGrXVuqlSe77fehTQGxCckTpaZ0HS3XfZNa/cas8JY
            /0RzH2n2AndnPirISDZ7r4ZIFuKAaivqaEkM8v7llI77URVB9ZJb/IqCrBzueAbt
            V/5ts2HpfBAUhw0RoiH8ql+IQZsuSOpRUC2gUN8460V4SQkVtDcsVTENiD+NM5Mg
            ImBv041CsW/rSJOilT2r/rWDN8RFnz/RrAQn+L31KXr81kg1TOLxO0ybs/eMJM3r
            RnHFZPiiKdqPlA60g0AnzKXPR2JTszHIgHHoRUW16I1WJeuAJNjg0JDQ0JM7pZ27
            JEaCc7uR12TPiuExKaNEaxKZVY1J0hzxOzF2MFIbAMVz/3K2ycvvuLxKojqIAXxA
            D+UtcOfJ62k2WnLXOEIZqFU0J2bvhxYUZOFS55wIn1UJF7hemD/LUFHBiWnuhwHk
            TAEl8M851t+Zp3hZeJzgx2kCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    p1nk = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.42.42";
          aliases = [
            "p1nk.r"
          ];
          tinc.pubkey = ''
            ----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA5YVML71oW3iJrzZKuX48
            AKrGitO5zNvsAHOI8BVsGfZTyxAAZgG4OaDX45kr27K39NcBU43LdDD0I1yjNvGe
            zAoL5MIiCPD/QR1kAvLmgpMUSqOVvrk+uoGLVt6dOGvxlOiG1AAaN0gA8Q0B/jZV
            4tZlBpZ7MX9xeK10wqVT56msN69P3EzKQn1uoVRrBxEnNvI1iqmmkgMLcrFVJFBQ
            888Uuw9Hx5MO7ES/ATe8mt0zReUGvn91jYVVsPpmAopWnjCol271gflY0RomFXKy
            XaIuvbeF+3otF0+MNqJfm4IsAKJjvl92pjVX0f0eBCSPCYR7D1EtgQrqflLkZKZ8
            jBGDlgpsFWt/Omz1BYcuGZU/djM4+SNxr4YRYMi3lMix3s2PmHvm304I7eEEBlC9
            qy1jq/sLaf8mHJrF6Htl7W5WS/Famkwv/VreI92iHrhsmIDiX7OIbXzYDCxT/PQa
            6uCm/3jIbcHG/ZHZ12H6thkafK0Aoe009+p1n+5Y7V2oNvYe3KzZTnCN5t6z1QHZ
            V5iypsd6lNDzlodjleTgGK8FmHGRPRdq1wb3eOLE8mWZj7ygDT50FwaC8FzAcHgC
            bLN/zlHvCbYmk9IJhktO3B6wtMrZl60+XCpb5rTulM94RirifFYsnTIDJApI11yb
            3AYi5dQXHjab/lvj6917xa0CAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
  };
  users = {
    mb = {
      mail = "mb0@codemonkey.cc";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDCHAdKGHP/De/GLEsPo5RBfbaiiitMw4Y/akOekJbImswT6Np2lzqno/WBJcfVs3D39wgPKNld4P/QZc5IwxC26q/PnBFu93KES0GqnlAqUNE63IOJ8UzNdyEqWggnRiLrBU+ZgyFZvmqp6NoSO4YEGEK4RZRMJM/GcAuQMj/nGjx2AHwPGZCkIRgz8/ctBOzX1/knZd3cOnNowH1wlqUKX6UcEzJdAVDQijHF1wl0Ri8tJKq9u8s/fw+1PSOpOHaeF1BALsXSKgeJDqUCTnZW5mAVUWJ86LvvyfCP4In9lhhLisbDm2cD96QaVvJyV6HfmegdSxZ1Phh+9Qz+3WhDJRedBTSKWfK/9j7VWSb+z/KV37q72W25ZfFMSay58LmCqn3v5fGt9qj4nlPw0By4baGLiGlA7xyvkJfdt8ZVPps5d2g6UprTbSA79lYN4qtWKq2Z9t317xch7Lix6EunQcoTkJ6QXEbDrAIk3zvkWr/CtpwEhNcSdWvQsua42dkD2oOI2F2IgFyYgOx9Iba2yj8A0TD2iqfYVhsJIYuk12QfeaR7ovQ6DhHlUxyQzeF6h0Y+I4AN6Sq/Mmj/cxfQoIaAEybUQMX+7KjFceIszT3JbGlz7DCxi7DMmNYuc7LELMRG3jNAOk+fW8u42Bhgc44tzvAondojerUGqCbUDw== mb0@codemonkey.cc";
    };
  };
}
