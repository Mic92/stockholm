{ config, pkgs, ... }:

{

  nixpkgs.config.packageOverrides = pkgs : {
    chromium = pkgs.chromium.override {
      pulseSupport = true;
    };
  };

  environment.systemPackages = with pkgs; [
    firefox
    chromium
  ];

  users.extraUsers = {
    firefox = {
      name = "firefox";
      description = "user for running firefox";
      home = "/home/firefox";
      useDefaultShell = true;
      extraGroups = [ "audio" ];
      createHome = true;
    };
    chromium = {
      name = "chromium";
      description = "user for running chromium";
      home = "/home/chromium";
      useDefaultShell = true;
      extraGroups = [ "audio" ];
      createHome = true;
    };
    facebook = {
      name = "facebook";
      description = "user for running facebook in chromium";
      home = "/home/facebook";
      useDefaultShell = true;
      extraGroups = [ "audio" ];
      createHome = true;
    };
    google = {
      name = "google";
      description = "user for running google+/gmail in chromium";
      home = "/home/google";
      useDefaultShell = true;
      createHome = true;
    };
    flash = {
      name = "flash";
      description = "user for running flash stuff";
      home = "/home/flash";
      useDefaultShell = true;
      extraGroups = [ "audio" ];
      createHome = true;
    };
  };

  security.sudo.extraConfig = ''
    lass ALL=(firefox) NOPASSWD: ALL
    lass ALL=(chromium) NOPASSWD: ALL
    lass ALL=(facebook) NOPASSWD: ALL
    lass ALL=(google) NOPASSWD: ALL
    lass ALL=(flash) NOPASSWD: ALL
  '';
}
