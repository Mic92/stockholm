{

  imports = [
    ./3modules
  ];

  nixpkgs = {
    overlays = [
      (import ../submodules/nix-writers/pkgs)
      (import ./5pkgs)
    ];
  };

}
