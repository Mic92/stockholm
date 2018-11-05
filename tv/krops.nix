{ name }: rec {

  inherit (import ../krebs/krops.nix { inherit name; })
    krebs-source
    lib
    pkgs
  ;

  source = lib.evalSource [
    krebs-source
    {
      nixos-config.symlink = "stockholm/tv/1systems/${name}/config.nix";
      secrets.file = toString ./dummy_secrets;
    }
  ];

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME --argstr target PATH -A test)
  test = { target }: pkgs.krops.writeTest "tv-krops-${name}-ci" {
    force = true;
    inherit source target;
  };

}
