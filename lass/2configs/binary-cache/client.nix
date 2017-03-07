{ config, ... }:

{
  nix = {
    binaryCaches = [
      "http://cache.prism.r"
      "https://cache.nixos.org/"
    ];
    binaryCachePublicKeys = [
      "cache.prism-1:+S+6Lo/n27XEtvdlQKuJIcb1yO5NUqUCE2lolmTgNJU="
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };
}

