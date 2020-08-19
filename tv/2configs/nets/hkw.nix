{
  krebs = {
    dns.providers.hkw = "hosts";
    hosts = {
      nomic = {
        nets.hkw = {
          ip4 = {
            addr = "10.23.1.110";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "nomic.hkw"
          ];
          ssh.port = 11423;
        };
      };
      ok = {
        external = true;
        nets.hkw = {
          ip4 = {
            addr = "10.23.1.1";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "ok.hkw"
          ];
        };
      };
      wu = {
        nets.hkw = {
          ip4 = {
            addr = "10.23.1.37";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "wu.hkw"
            "cache.wu.hkw"
          ];
          ssh.port = 11423;
        };
      };
      xu = {
        nets.hkw = {
          ip4 = {
            addr = "10.23.1.38";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "xu.hkw"
            "cache.xu.hkw"
          ];
          ssh.port = 11423;
        };
      };
      zu = {
        nets.hkw = {
          ip4 = {
            addr = "10.23.1.39";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "zu.hkw"
          ];
          ssh.port = 11423;
        };
      };
    };
  };
}
