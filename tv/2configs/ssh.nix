with import ./lib;
{ config, pkgs, ... }: {
  # Override NixOS's "Allow DSA keys for now."
  environment.etc."ssh/ssh_config".text = mkForce ''
    AddressFamily ${if config.networking.enableIPv6 then "any" else "inet"}

    ${optionalString config.programs.ssh.setXAuthLocation ''
      XAuthLocation ${pkgs.xorg.xauth}/bin/xauth
    ''}

    ForwardX11 ${if config.programs.ssh.forwardX11 then "yes" else "no"}

    ${config.programs.ssh.extraConfig}
  '';

  programs.ssh = {
    extraConfig = ''
      UseRoaming no
    '';
    startAgent = false;
  };
}
