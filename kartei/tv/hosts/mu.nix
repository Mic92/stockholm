{
  nets = {
    retiolum = {
      ip4.addr = "10.243.20.1";
      aliases = [
        "mu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEApXErmPSn2CO4V25lqxanCGCFgxEAjdzFUiTCCu0IvELEuCc3PqVA
        g4ecf8gGwPCbzMW/1txjlgbsQcm87U5enaCwzSv/pa7P9/memV74OhqEVOypFlDE
        XeZczqQfNbjoLYl4cKZpTsSZmOgASXaMDrH2N37f50q35C0MQw0HRzaQM5VLrzb4
        o87MClS+yPqpvp34QjW+1lqnOKvMkr6mDrmtcAjCOs9Ma16txyfjGVFi8KmYqIs1
        QEJmyC9Uocz5zuoSLUghgVRn9yl4+MEw6++akFDwKt/eMkcSq0GPB+3Rz/WLDiBs
        FK6BsssQWdwiEWpv6xIl1Fi+s7F0riq2cwIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      #tinc.pubkey_ed25519 = "cEf/Kq/2Fo70yoIcVmhIp4it9eA7L3GdkgrVE9AWU6C";
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1vJsAddvxMA84u9iJEOrIkKn7pQiemMbfW5cfK1d7g root@mu";
}
