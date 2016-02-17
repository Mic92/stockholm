{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  rules = with git; singleton {
    user = [ wolf-repo-sync ];
    repo = [ stockholm-mirror ];
    perm = push ''refs/*'' [ non-fast-forward create delete merge ];
  };

  stockholm-mirror = {
    public = true;
    name = "stockholm-mirror";
    desc = "mirror for all stockholm branches";
    hooks = {
      post-receive = pkgs.git-hooks.irc-announce {
        nick = config.networking.hostName;
        verbose = false;
        channel = "#retiolum";
        server = "cd.retiolum";
      };
    };
  };

  wolf-repo-sync = {
    name = "wolf-repo-sync";
    mail = "spam@krebsco.de";
    # TODO put git-sync pubkey somewhere more appropriate
    pubkey = ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCwuAZB3wtAvBJFYh+gWdyGaZU4mtqM2dFXmh2rORlbXeh02msu1uv07ck1VKkQ4LgvCBcBsAOeVa1NTz99eLqutwgcqMCytvRNUCibcoEWwHObsK53KhDJj+zotwlFhnPPeK9+EpOP4ngh/tprJikttos5BwBwe2K+lfiid3fmVPZcTTYa77nCwijimMvWEx6CEjq1wiXMUc4+qcEn8Swbwomz/EEQdNE2hgoC3iMW9RqduTFdIJWnjVi0KaxenX9CvQRGbVK5SSu2gwzN59D/okQOCP6+p1gL5r3QRHSLSSRiEHctVQTkpKOifrtLZGSr5zArEmLd/cOVyssHQPCX repo-sync@wolf'';
  };

in {
  krebs.users.wolf-repo-sync = wolf-repo-sync;
  krebs.git = {
    enable = true;
    root-title = "Shared Repos";
    root-desc = "keep on krebsing";
    inherit rules;
    repos.stockholm-mirror = stockholm-mirror;
  };
}
