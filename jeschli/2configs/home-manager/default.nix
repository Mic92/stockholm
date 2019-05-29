{
  imports = [
    <home-manager/nixos>
  ];
  home-manager.useUserPackages = true;
  home-manager.users.jeschli = {
    home.stateVersion = "19.03";
  };
}
