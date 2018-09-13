with import <stockholm/lib>;
import <nixpkgs/nixos/tests/make-test.nix> ({ ... }:

let
  pkgs = import <nixpkgs> { overlays = [(import ../5pkgs)]; };
  test-config = <stockholm/krebs/0tests/data/test-config.nix>;
  privKey = ''
    -----BEGIN OPENSSH PRIVATE KEY-----
    b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
    QyNTUxOQAAACD1tYD8r6Fcd7bq3Z0nvo5483nXQ8c4LFh0fcw8rOCQtQAAAJBTNHK6UzRy
    ugAAAAtzc2gtZWQyNTUxOQAAACD1tYD8r6Fcd7bq3Z0nvo5483nXQ8c4LFh0fcw8rOCQtQ
    AAAECK2ZlEIofZyGbh7rXlUq5lUsUyotamtp9QrlvoS3qgePW1gPyvoVx3turdnSe+jnjz
    eddDxzgsWHR9zDys4JC1AAAACWxhc3NAbW9ycwECAwQ=
    -----END OPENSSH PRIVATE KEY-----
  '';
  pubKey = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPW1gPyvoVx3turdnSe+jnjzeddDxzgsWHR9zDys4JC1
  '';

  ssh-config = pkgs.writeText "ssh-config" ''
    Host server
        StrictHostKeyChecking no
        UserKnownHostsFile=/dev/null
  '';

  populate-source = {
    nixos-config = {
      symlink.target = test-config;
      type = "symlink";
    };
    nixpkgs = {
      symlink.target = <nixpkgs>;
      type = "symlink";
    };
    stockholm = {
      symlink.target = <stockholm>;
      type = "symlink";
    };
  };

  test-deploy = pkgs.writeDash "test-deploy" ''
    cd ${<stockholm>}
    export NIX_PATH=stockholm=${<stockholm>}:nixpkgs=${<nixpkgs>}:$NIX_PATH
    exec >&2
    source=${pkgs.writeJSON "source.json" populate-source}
    LOGNAME=krebs ${pkgs.populate}/bin/populate --force root@server:22/var/src/ < "$source"
  '';
  minimalSystem = (import <nixpkgs/nixos/lib/eval-config.nix> {
    modules = [
      test-config
    ];
  }).config.system.build.toplevel;

in {
  name = "deploy";

  nodes = {

    server =
      { config, pkgs, ... }:

      {
        imports = [ test-config ];
        environment.variables = {
          NIX_PATH = mkForce "nixpkgs=${<nixpkgs>}";
        };
        services.openssh.enable = true;
        users.extraUsers.root.openssh.authorizedKeys.keys = [
          pubKey
        ];
        virtualisation.pathsInNixDB = [
          minimalSystem
        ];
        environment.systemPackages = [ pkgs.git ];
      };

    client =
      { config, pkgs, ... }:
      { };
  };

  testScript = ''
    startAll;

    $server->waitForUnit("sshd");

    $client->succeed("mkdir -p -m 700 /root/.ssh");
    $client->succeed("echo '${privKey}' > /root/.ssh/id_ed25519");
    $client->succeed("cp ${ssh-config} /root/.ssh/config");
    $client->succeed("chmod 600 /root/.ssh/id_ed25519");

    $server->waitForUnit("network.target");
    $server->succeed("ip route show 1>&2");
    $client->waitForUnit("network.target");
    $client->succeed("${test-deploy}");
    $server->succeed("nixos-rebuild -I /var/src switch");

    $client->shutdown;
    $server->shutdown;
  '';
})
