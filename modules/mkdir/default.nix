{ pkgs, ... }:

let
  inherit (builtins) readFile;
in

{
  imports =
    [
      <secrets/hashedPasswords.nix>
      ./networking.nix
      ./users.nix
      ../common/nixpkgs.nix
      ../tv/base.nix
      ../tv/base-cac-CentOS-7-64bit.nix
      ../tv/exim-smarthost.nix
      ../tv/git/public.nix
      ../tv/sanitize.nix
      {
        imports = [ ../tv/iptables ];
        tv.iptables = {
          enable = true;
          input-internet-accept-new-tcp = [
            "ssh"
            "tinc"
            "smtp"
            "xmpp-client"
            "xmpp-server"
          ];
          input-retiolum-accept-new-tcp = [
            "http"
          ];
        };
      }
      {
        imports = [ ../tv/retiolum ];
        tv.retiolum = {
          enable = true;
          hosts = <retiolum-hosts>;
          connectTo = [
            "cd"
            "fastpoke"
            "pigstarter"
            "ire"
          ];
        };
      }
    ];

  nix.maxJobs = 1;

  nixpkgs = {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "4c01e6d91993b6de128795f4fbdd25f6227fb870";
  };

  environment.systemPackages = with pkgs; [
    git # required for ./deploy, clone_or_update
    htop
    iftop
    iotop
    iptables
    mutt    # for mv
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];

  security.rtkit.enable = false;

  services.cron.enable = false;

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  services.ntp.enable = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
    permitRootLogin = "yes";
  };

  sound.enable = false;
}
