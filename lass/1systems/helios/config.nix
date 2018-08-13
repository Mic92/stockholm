with import <stockholm/lib>;
{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/pass.nix>
    {
      services.xserver.dpi = 200;
      fonts.fontconfig.dpi = 200;
      lass.fonts.regular = "xft:Hack-Regular:pixelsize=22,xft:Symbola";
      lass.fonts.bold =    "xft:Hack-Bold:pixelsize=22,xft:Symbola";
      lass.fonts.italic =  "xft:Hack-RegularOblique:pixelsize=22,xft:Symbol";
    }
  ];
  krebs.build.host = config.krebs.hosts.helios;

  environment.systemPackages = with pkgs; [
    ag
    vim
    git
    rsync
    hashPassword
    thunderbird
    dpass

    # we want tensorflow! (with GPU acceleration)
    python3Packages.tensorflowWithCuda
  ];

  users.users = {
    root.openssh.authorizedKeys.keys = [
      config.krebs.users.lass-helios.pubkey
    ];
  };

  services.tlp.enable = true;

  services.printing.drivers = [ pkgs.postscript-lexmark ];

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

}
