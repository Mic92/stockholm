{buildbot-nix,...}:
let
  #domain = "buildbot.krebsco.de";
  domain = "build.hotdog.r";
in {
  imports = [
    buildbot-nix.nixosModules.buildbot-master
  ];

  #services.nginx.virtualHosts."${domain}" = {
  #  enableACME = true;
  #  forceSSL = true;
  #};


  services.buildbot-nix.master = {
    enable = true;
    admins = [ "makefu" ];
    buildSystems = [ "x86_64-linux" "aarch64-linux" ];
    inherit domain;
    evalMaxMemorySize = "4096";
    evalWorkerCount = 16;
    workersFile = "/var/src/secrets/buildbot/nix-workers";
    github = {
      tokenFile = "/var/src/secrets/buildbot/github-token";
      webhookSecretFile = "/var/src/secrets/buildbot/github-webhook-secret";
      oauthSecretFile = "/var/src/secrets/buildbot/github-oauth-secret";
      oauthId = "Ov23lizFP7t7qoE9FuDA";
      user = "krebs-bob";
      topic = "buildbot";
    };
  };
}
