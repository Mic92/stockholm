{

  imports = [
    ./3modules
  ];

  nixpkgs = {
    overlays = [
      (import ./5pkgs)
      (import ../submodules/nix-writers/pkgs)
    ];
  };

}
