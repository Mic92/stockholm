{
  krebs = {
    dns.providers.gg23 = "hosts";
    hosts = {
      nomic = {
        nets.gg23 = {
          ip4 = {
            addr = "10.23.1.110";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "nomic.gg23"
          ];
          ssh.port = 11423;
        };
      };
      ok = {
        external = true;
        nets.gg23 = {
          ip4 = {
            addr = "10.23.1.1";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "ok.gg23"
          ];
        };
      };
      wu = {
        nets.gg23 = {
          ip4 = {
            addr = "10.23.1.37";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "wu.gg23"
            "cache.wu.gg23"
          ];
          ssh.port = 11423;
        };
      };
      xu = {
        nets.gg23 = {
          ip4 = {
            addr = "10.23.1.38";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "xu.gg23"
            "cache.xu.gg23"
          ];
          ssh.port = 11423;
        };
      };
      zu = {
        nets.gg23 = {
          ip4 = {
            addr = "10.23.1.39";
            prefix = "10.23.1.0/24";
          };
          aliases = [
            "zu.gg23"
          ];
          ssh.port = 11423;
        };
      };
    };
  };
}
