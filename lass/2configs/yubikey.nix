{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-manager
  ];

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;
  systemd.user.sockets.gpg-agent-ssh.wantedBy = [ "sockets.target" ];

  ##restart pcscd if yubikey is plugged in
  #services.udev.extraRules = ''
  #  ACTION=="add", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="2013", RUN+="${pkgs.writeDash "restart_pcscd" ''
  #    ${pkgs.systemd}/bin/systemctl restart pcscd.service
  #  ''}"
  #'';

  environment.shellInit = ''
    if [ "$UID" -eq 1337 ] && [ -z "$SSH_CONNECTION" ]; then
      export GPG_TTY="$(tty)"
      gpg-connect-agent --quiet updatestartuptty /bye > /dev/null
      export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
      if [ -z "$SSH_AUTH_SOCK" ]; then
        export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
      fi

    fi
  '';

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      # enableSSHSupport = true;
    };
  };
}
