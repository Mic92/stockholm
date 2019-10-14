{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
  ];

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;

  environment.shellInit = ''
    if [ "$UID" -eq 1337 ]; then
      export GPG_TTY="$(tty)"
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    fi
  '';

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
