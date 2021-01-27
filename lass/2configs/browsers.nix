{ config, lib, pkgs, ... }:
{
  lass.browser.config = {
    cr = { groups = [ "audio" "video" ]; precedence = 9; };
  };
  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
    ];
  };
}
