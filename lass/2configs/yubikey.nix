{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-manager
  ];

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  systemd.user.sockets.gpg-agent-ssh.wantedBy = [ "sockets.target" ];

  services.pcscd.enable = true;
  systemd.user.services.gpg-agent.serviceConfig.ExecStartPre = pkgs.writers.writeDash "init_gpg" ''
    set -x
    ${pkgs.coreutils}/bin/ln -sf ${pkgs.writeText "scdaemon.conf" ''
      disable-ccid
      pcsc-driver ${pkgs.pcsclite.out}/lib/libpcsclite.so.1
      card-timeout 1

      # Always try to use yubikey as the first reader
      # even when other smart card readers are connected
      # Name of the reader can be found using the pcsc_scan command
      # If you have problems with gpg not recognizing the Yubikey
      # then make sure that the string here matches exacly pcsc_scan
      # command output. Also check journalctl -f for errors.
      reader-port Yubico YubiKey
    ''} $HOME/.gnupg/scdaemon.conf
  '';

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (
        (
          action.id == "org.debian.pcsc-lite.access_pcsc" ||
          action.id == "org.debian.pcsc-lite.access_card"
        ) && subject.user == "lass"
      ) {
        return polkit.Result.YES;
      }
    });
    polkit.addRule(function(action, subject) {
     polkit.log("subject: " + subject + " action: " + action);
    });
  '';

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

  # allow nix to acces remote builders via yubikey
  systemd.services.nix-daemon.environment.SSH_AUTH_SOCK = "/run/user/1337/gnupg/S.gpg-agent.ssh";

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      # enableSSHSupport = true;
    };
  };
}
