{ cfg, config, lib, pkgs, ... }:

let
  inherit (builtins) attrValues filter getAttr;
  inherit (lib) concatMapStringsSep mkIf optionalString;

  location = lib.nameValuePair; # TODO this is also in modules/wu/default.nix

  isPublicRepo = getAttr "public"; # TODO this is also in ./default.nix
in

{
  users.extraUsers = lib.singleton {
    name = "fcgiwrap";
    uid = 2851179180; # genid fcgiwrap
    group = "fcgiwrap";
    home = toString (pkgs.runCommand "empty" {} "mkdir -p $out");
  };

  users.extraGroups = lib.singleton {
    name = "fcgiwrap";
    gid = 2851179180; # genid fcgiwrap
  };

  services.fcgiwrap = {
    enable = true;
    user = "fcgiwrap";
    group = "fcgiwrap";
    # socketAddress = "/run/fcgiwrap.sock" (default)
    # socketType = "unix" (default)
  };

  environment.etc."cgitrc".text = ''
    css=/cgit-static/cgit.css
    logo=/cgit-static/cgit.png

    # if you do not want that webcrawler (like google) index your site
    robots=noindex, nofollow

    virtual-root=/cgit

    # TODO make this nicer
    cache-root=/tmp/cgit

    cache-size=1000
    enable-commit-graph=1
    enable-index-links=1
    enable-index-owner=0
    enable-log-filecount=1
    enable-log-linecount=1
    enable-remote-branches=1

    root-title=public repositories at ${config.networking.hostName}
    root-desc=keep calm and engage

    snapshots=0
    max-stats=year

    ${concatMapStringsSep "\n" (repo: ''
      repo.url=${repo.name}
      repo.path=${cfg.dataDir}/${repo.name}
      ${optionalString (repo.section != null) "repo.section=${repo.section}"}
      ${optionalString (repo.desc != null) "repo.desc=${repo.desc}"}
    '') (filter isPublicRepo (attrValues cfg.repos))}
  '';

  tv.nginx = {
    enable = true;
    retiolum-locations = [
      (location "/cgit/" ''
        include             ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param       SCRIPT_FILENAME ${pkgs.cgit}/cgit/cgit.cgi;
        fastcgi_split_path_info ^(/cgit/?)(.+)$;
        fastcgi_param       PATH_INFO       $fastcgi_path_info;
        fastcgi_param       QUERY_STRING    $args;
        fastcgi_param       HTTP_HOST       $server_name;
        fastcgi_pass        unix:${config.services.fcgiwrap.socketAddress};
      '')
      (location "= /cgit" ''
        return 301 /cgit/;
      '')
      (location "/cgit-static/" ''
        root ${pkgs.cgit}/cgit;
        rewrite ^/cgit-static(/.*)$ $1 break;
      '')
    ];
  };
}
