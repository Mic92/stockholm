{ pkgs, ... }:

{
  home.file = {
     ".emacs.d" = {
       source = pkgs.fetchFromGitHub {
         owner = "jeschli";
         repo = "emacs.d";
         rev = "8ed6c40";
         sha256 = "1q2y478srwp9f58l8cixnd2wj51909gp1z68k8pjlbjy2mrvibs0";
       };
       recursive = true;
     };
     ".config/i3/config".text = ''
     '';
  }; 
  
}
