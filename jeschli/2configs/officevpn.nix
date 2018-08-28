{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = pkgs: {
    openvpn = pkgs.openvpn.override { pkcs11Support = true; useSystemd = false;};
  };

  environment.systemPackages = with pkgs; [
  opensc
  openvpn
  yubikey-manager
  ];

  services.pcscd.enable = true;

  # To start the vpn manually execute
  # $ openvpn --config clien.ovpn
}

