{ lib, ... }: {
  services.openssh.knownHosts.github = {
    hostNames =
      ["github.com"]
      ++
      # List generated with (IPv6 addresses are currently ignored):
      # curl -sS https://api.github.com/meta | jq -r .git[] | grep -v : | nix-shell -p cidr2glob --run cidr2glob | jq -Rs 'split("\n")|map(select(.!=""))' > known-hosts.json
      lib.importJSON ./known-hosts.json
    ;
    publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
  };
}
