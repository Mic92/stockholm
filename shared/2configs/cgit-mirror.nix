{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  rules = with git; singleton {
    user = [ git-sync ];
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

  git-sync = {
    name = "git-sync";
    mail = "spam@krebsco.de";
    # TODO put git-sync pubkey somewhere more appropriate
    pubkey = ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzUuzyoAhMgJmsiaTVWNSXqcrZNTpKpv0nfFBOMcNXUWEbvfAq5eNpg5cX+P8eoYl6UQgfftbYi06flKK3yJdntxoZKLwJGgJt9NZr8yZTsiIfMG8XosvGNQtGPkBtpLusgmPpu7t2RQ9QrqumBvoUDGYEauKTslLwupp1QeyWKUGEhihn4CuqQKiPrz+9vbNd75XOfVZMggk3j4F7HScatmA+p1EQXWyq5Jj78jQN5ZIRnHjMQcIZ4DOz1U96atwSKMviI1xEZIODYfgoGjjiWYeEtKaLVPtSqtLRGI7l+RNouMfwHLdTWOJSlIdFncfPXC6R19hTll3UHeHLtqLP git-sync'';
  };

in {
  krebs.git = {
    enable = true;
    root-title = "Shared Repos";
    root-desc = "keep on krebsing";
    inherit rules;
    repos.stockholm-mirror = stockholm-mirror;
  };
}
