{
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.44";
      aliases = [
        "fu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEA7zwE/2k+c14PkDPaDF4Ss4oxIvb99kcim9qHHhHanZKS0SG0pEOB
        UthaL8ZC3ww278eh6J1hLsaqJsznEs7TAFYZtH94lbXyxsGq3hdlpMhXKdgeHuei
        ZpNj/gyo1REsHz4k4Xj3XmtqWoAteQviccl2zi+KcC0U9hxvbnXIY3CGYgNsCFb4
        2EJtFXi2nDoHXicso2+bUufIhNGjxEkye9dEkChEGM27fxSr61yVlLARpm67jfEY
        kTW2OXOYz1yJ6Akr4yvQaS3FN6sEQ3YbE57Xju46VHn5kOmpYVMGyktmdOZwHnaO
        iaTLEzuYBEAJuyEt/2/XmiCGjlxrIGkyZQIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "a2nUW601al1Sp1owDC4D3ukDesHThXeabMzhUckUL1O";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE8T+2Oe6qCE0uEb9H7CWZengyhHK30NelmYmpI4Umpm root@fu";
  syncthing.id = "F5B3EPT-OEOFYMV-GATESYO-727M6R4-YBXGW6Q-SG3QWC7-PPVFX4C-AY4UKAJ";
}
