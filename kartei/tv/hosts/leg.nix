{
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.43";
      aliases = [
        "leg.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAsfL4VK3WbgbWVYsOA0TJ3iswRrvfE/z/TbNTtzULGPSA6bTG8QXO
        f2cm6aY6UriMktJL6GB3XNYlDZDKi74bNOXP+O/p7dTr5g9PWjYeqLFiLFr0pwWi
        pooKxrAcPEJ8khhCI7eXVGL1baiHZsPCZLmPXm+c3qke6uY/48zmt0SG3WwjybF/
        JMbxE7XTMrsO28PiOZgWrXqZJgLhKygcz9WGMkQ9CcjnHobKIoTRWHILIsEPjR2s
        /vNeGTa6v9/SpDQtHlfiELNxQAHUXU0//hJvEyH4dMS+vJKNQlL9z84fQqhZGfh0
        nN++k9cHwSPDusbMqB2ncpx6v8ieUpCsewIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = " qmxNtjkjzXP4QCIJwXLncYFrIfU7royMlQNSVvR3XKH";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGiputkYYQbg8sUHu+dMVOEuqhPYwPhPdmkS6LopPx17 root@leg";
  syncthing.id = "5IB2U3K-HNQWNA4-ULYNPZF-XC3HX4D-IKQB72L-GNF6U2P-RNL4OMF-BWGDVAU";
}
