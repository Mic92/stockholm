{

  imports = [
    ../krebs
    ./2configs
    ./3modules
  ];

  nixpkgs = {
    overlays = [
      (import ./5pkgs)
    ];
  };

}
