{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {

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
    ${mainUser.name} ALL=(firefox) NOPASSWD: ALL
    ${mainUser.name} ALL=(chromium) NOPASSWD: ALL
    ${mainUser.name} ALL=(facebook) NOPASSWD: ALL
    ${mainUser.name} ALL=(google) NOPASSWD: ALL
    ${mainUser.name} ALL=(flash) NOPASSWD: ALL
  '';
}
