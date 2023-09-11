{ config, ... }: {
  boot.initrd.availableKernelModules = [
    "e1000e"
  ];
  boot.initrd.network.enable = true;
  boot.initrd.network.ssh = {
    enable = true;
    port = 11423;
    authorizedKeys = [
      config.krebs.users.tv.pubkey
    ];
    ignoreEmptyHostKeys = true;
  };
  boot.initrd.secrets = {
    "/etc/ssh/ssh_host_rsa_key" = "${config.krebs.secret.directory}/initrd/ssh_host_rsa_key";
  };
}
