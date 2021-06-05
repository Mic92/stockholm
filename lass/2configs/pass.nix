{ config, pkgs, ... }:

{
  users.users.mainUser.packages = with pkgs; [
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    gnupg
  ];

  programs.gnupg.agent.enable = true;
}
