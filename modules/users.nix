{ config, pkgs, ... }:

let
  inherit (builtins) attrValues;
  inherit (pkgs.lib) concatMap filterAttrs mapAttrs concatStringsSep;


  users = {
    tv = {
      uid = 1337;
      group = "users";
      extraGroups = [
        "audio"
        "video"
        "wheel"
      ];
    };

    ff = {
      uid = 13378001;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
      ];
    };

    cr = {
      uid = 13378002;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
        "bumblebee"
      ];
    };

    vimb = {
      uid = 13378003;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
        "bumblebee"
      ];
    };

    fa = {
      uid = 2300001;
      group = "tv-sub";
    };

    rl = {
      uid = 2300002;
      group = "tv-sub";
    };

    btc-bitcoind = {
      uid = 2301001;
      group = "tv-sub";
    };

    btc-electrum = {
      uid = 2301002;
      group = "tv-sub";
    };

    ltc-litecoind = {
      uid = 2301101;
      group = "tv-sub";
    };

    eth = {
      uid = 2302001;
      group = "tv-sub";
    };

    emse-hsdb = {
      uid = 4200101;
      group = "tv-sub";
    };

    wine = {
      uid = 13370400;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
        "bumblebee"
      ];
    };

    # dwarffortress
    df = {
      uid = 13370401;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
        "bumblebee"
      ];
    };

    # XXX visudo: Warning: Runas_Alias `FTL' referenced but not defined
    FTL = {
      uid = 13370402;
      #group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
        "bumblebee"
      ];
    };

    freeciv = {
      uid = 13370403;
      group = "tv-sub";
    };

    xr = {
      uid = 13370061;
      group = "tv-sub";
      extraGroups = [
        "audio"
        "video"
      ];
    };

    "23" = {
      uid = 13370023;
      group = "tv-sub";
    };

    electrum = {
      uid = 13370102;
      group = "tv-sub";
    };

    Reaktor = {
      uid = 4230010;
      group = "tv-sub";
    };

    gitolite = {
      uid = 7700;
    };

    skype = {
      uid = 6660001;
      group = "tv-sub";
      extraGroups = [
        "audio"
      ];
    };

    onion = {
      uid = 6660010;
      group = "tv-sub";
    };

    zalora = {
      uid = 1000301;
      group = "tv-sub";
      extraGroups = [
        "audio"
        # TODO remove vboxusers when hardening is active
        "vboxusers"
        "video"
      ];
    };

  };


  extraUsers =
    mapAttrs (name: user: user // {
      inherit name;
      home = "/home/${name}";
      createHome = true;
      useDefaultShell = true;
    }) users;


  extraGroups = {
    tv-sub.gid = 1337;
  };


  sudoers =
    let
      inherit (builtins) filter hasAttr;
      inherit (import ../lib { inherit pkgs; }) concat isSuffixOf removeSuffix setToList;

      hasMaster = { group ? "", ... }:
        isSuffixOf "-sub" group;

      masterOf = user : removeSuffix "-sub" user.group;
    in
    concatStringsSep "\n"
      (map (u: "${masterOf u} ALL=(${u.name}) NOPASSWD: ALL")
           (filter hasMaster (attrValues extraUsers)));

in


{
  imports = [
    <secrets/hashedPasswords.nix>
  ];

  users.defaultUserShell = "/run/current-system/sw/bin/bash";
  users.extraGroups = extraGroups;
  users.extraUsers = extraUsers;
  users.mutableUsers = false;

  #security.sudo.configFile = sudoers config.users.extraUsers;
  security.sudo.configFile =
    with builtins; trace
    ''
      OK
    ''
    sudoers;
  security.sudo.extraConfig = ''
    Defaults mailto="tv@wu.retiolum"
  '';
}
