{ config, pkgs, ... }:

{
  users.users.lass.packages = with pkgs; [
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    gnupg
  ];

  programs.gnupg.agent.enable = true;
}
