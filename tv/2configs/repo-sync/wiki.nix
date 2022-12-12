with import ./lib;
{ config, pkgs, ... }: {
  krebs.repo-sync.enable = true;
  krebs.repo-sync.repos.wiki.branches.hotdog = {
    origin.url = "http://cgit.hotdog.r/wiki";
    mirror.url = "git@${config.krebs.build.host.name}.r:wiki";
  };
  krebs.git.repos.wiki = {
    public = true;
    name = "wiki";
    cgit.desc = toString [
      "mirror of"
      config.krebs.repo-sync.repos.wiki.branches.hotdog.origin.url
    ];
    cgit.section = "7. mirrors";
    hooks.post-receive = /* sh */ ''
      ${pkgs.git-hooks.irc-announce {
        channel = "#xxx";
        nick = config.krebs.build.host.name;
        server = "irc.r";
      }}
      ${pkgs.cgit-clear-cache.override {
        inherit (config.krebs.git.cgit.settings) cache-root;
      }}/bin/cgit-clear-cache
    '';
  };
  krebs.git.rules = lib.singleton {
    user = lib.singleton config.krebs.users.repo-sync;
    repo = lib.singleton config.krebs.git.repos.wiki;
    perm = lib.git.push "refs/*" [
      lib.git.create
      lib.git.delete
      lib.git.merge
      lib.git.non-fast-forward
    ];
  };
  krebs.users.${config.krebs.repo-sync.user.name}.pubkey = {
    ni = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK9U0Ob9/O0kxg3trhZY/vDnbqfN+R5cASGiClRr4IM";
  }.${config.krebs.build.host.name};
}
