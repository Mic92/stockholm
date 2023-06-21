args: {

  imports = [
    ./3modules
  ];

  nixpkgs = {
    overlays = [
      (import ((args.nix-writers or ../submodules/nix-writers) + "/pkgs"))
      (import ./5pkgs)
    ];
  };

}
