{ pkgs, ... }:

let
  inherit (builtins) readFile;
in

{
  imports =
    [
      { users.extraUsers = import <secrets/extraUsers.nix>; }
      ./networking.nix
      ./users.nix
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
            "mkdir"
            "fastpoke"
            "pigstarter"
            "ire"
          ];
        };
      }
    ];

  nix.maxJobs = 1;

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

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

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
