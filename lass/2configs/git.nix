{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let

  out = {
    services.nginx.enable = true;
    krebs.git = {
      enable = true;
      cgit = {
        settings = {
          root-title = "public repositories at ${config.krebs.build.host.name}";
          root-desc = "keep calm and engage";
        };
      };
      repos = repos;
      rules = rules;
    };

    krebs.iptables.tables.filter.INPUT.rules = [
      { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; }
    ];
  };

  repos =
    public-repos //
    optionalAttrs config.krebs.build.host.secure restricted-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    news = {
      cgit.desc = "take a rss feed and a timeout and print it to stdout";
      cgit.section = "software";
    };
    nixpkgs = {
      cgit.desc = "nixpkgs fork";
      cgit.section = "configuration";
    };
    populate = {
      cgit.section = "software";
    };
    stockholm = {
      cgit.desc = "take all the computers hostage, they'll love you!";
      cgit.section = "configuration";
    };
    stockholm-issues = {
      cgit.desc = "stockholm issues";
      cgit.section = "issues";
    };
    the_playlist = {
      cgit.desc = "Good Music collection + tools";
      cgit.section  = "art";
    };
    nix-user-chroot = {
      cgit.desc = "Fork of nix-user-chroot my lethalman";
      cgit.section = "software";
    };
    krops = {
      cgit.desc = "krebs deployment";
      cgit.section = "software";
    };
  } // mapAttrs make-public-repo-silent {
    nixos-aws = {
      collaborators = [ {
        name = "fabio";
        pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFst8DvnfOu4pQJYxcwdf//jWTvP+jj0iSrOdt59c9Gbujm/8K1mBXhcSQhHj/GBRC1Qc1wipf9qZsWnEHMI+SRwq6tDr8gqlAcdWmHAs1bU96jJtc8EgmUKbXTFG/VmympMPi4cEbNUtH93v6NUjQKwq9szvDhhqSW4Y8zE32xLkySwobQapNaUrGAtQp3eTxu5Lkx+cEaaartaAspt8wSosXjUHUJktg0O5/XOP+CiWAx89AXxbQCy4XTQvUExoRGdw9sdu0lF0/A0dF4lFF/dDUS7+avY8MrKEcQ8Fwk8NcW1XrKMmCdNdpvou0whL9aHCdTJ+522dsSB1zZWh63Si4CrLKlc1TiGKCXdvzmCYrD+6WxbPJdRpMM4dFNtpAwhCm/dM+CBXfDkP0s5veFiYvp1ri+3hUqV/sep9r5/+d+5/R1gQs8WDNjWqcshveFbD5LxE6APEySB4QByGxIrw7gFbozE+PNxtlVP7bq4MyE6yIzL6ofQgO1e4THquPcqSCfCvyib5M2Q1phi5DETlMemWp84AsNkqbhRa4BGRycuOXXrBzE+RgQokcIY7t3xcu3q0xJo2+HxW/Lqi72zYU1NdT4nJMETEaG49FfIAnUuoVaQWWvOz8mQuVEmmdw2Yzo2ikILYSUdHTp1VPOeo6aNPvESkPw1eM0xDRlQ== ada";
      } ];
    };
  };

  restricted-repos = mapAttrs make-restricted-repo (
    {
      brain = {
        collaborators = with config.krebs.users; [ tv makefu ];
        announce = true;
      };
    } //
    import <secrets/repos.nix> { inherit config lib pkgs; }
  );

  make-public-repo = name: { cgit ? {}, collaborators ? [], ... }: {
    inherit cgit collaborators name;
    public = true;
    hooks = {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#xxx";
        server = "irc.r";
        verbose = config.krebs.build.host.name == "prism";
        # TODO define branches in some kind of option per repo
        branches = [ "master" ];
      };
    };
  };

  make-public-repo-silent = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
  };

  make-restricted-repo = name: { admins ? [], collaborators ? [], announce ? false, hooks ? {}, ... }: {
    inherit admins collaborators name;
    public = false;
    hooks = optionalAttrs announce {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#xxx";
        server = "irc.r";
        verbose = false;
        # TODO define branches in some kind of option per repo
        branches = [ "master" "staging*" ];
      };
    } // hooks;
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = [ lass lass-shodan ];
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional (length (repo.admins or []) > 0) {
        user = repo.admins;
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional (length (repo.collaborators or []) > 0) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      } ++
      optional repo.public {
        user = attrValues config.krebs.users;
        repo = [ repo ];
        perm = fetch;
      };

in out
