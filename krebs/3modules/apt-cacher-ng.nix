{ config, pkgs, lib, ... }:

with lib;
let
  acng-config = pkgs.writeTextFile {
    name = "acng-configuration";
    destination = "/acng.conf";
    text = ''
      ForeGround: 1
      CacheDir: ${cfg.cacheDir}
      LogDir: ${cfg.logDir}
      PidFile: /var/run/apt-cacher-ng.pid
      ExTreshold: ${toString cfg.cacheExpiration}
      CAfile: ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt

      Port: ${toString cfg.port}
      BindAddress: ${cfg.bindAddress}

      # defaults:
      Remap-debrep: file:deb_mirror*.gz /debian ; file:backends_debian
      Remap-uburep: file:ubuntu_mirrors /ubuntu ; file:backends_ubuntu
      Remap-debvol: file:debvol_mirror*.gz /debian-volatile ; file:backends_debvol
      Remap-cygwin: file:cygwin_mirrors /cygwin
      Remap-sfnet:  file:sfnet_mirrors
      Remap-alxrep: file:archlx_mirrors /archlinux
      Remap-fedora: file:fedora_mirrors
      Remap-epel:   file:epel_mirrors
      Remap-slrep:  file:sl_mirrors # Scientific Linux
      Remap-gentoo: file:gentoo_mirrors.gz /gentoo ; file:backends_gentoo

      ReportPage: acng-report.html
      SupportDir: ${pkgs.apt-cacher-ng}/lib/apt-cacher-ng
      LocalDirs: acng-doc ${pkgs.apt-cacher-ng}/share/doc/apt-cacher-ng

      # Nix cache
      ${optionalString cfg.enableNixCache ''
        Remap-nix: http://cache.nixos.org /nixos ; https://cache.nixos.org
        PfilePatternEx: (^|.*?/).*\.nar(info)?(|\.gz|\.xz|\.bz2)$
        VfilePatternEx: (^|.*?/)nix-cache-info$
      ''}

      ${cfg.extraConfig}
    '';
  };

  acng-home = "/var/cache/acng";
  cfg = config.krebs.apt-cacher-ng;

  api = {
    enable = mkEnableOption "apt-cacher-ng";

    cacheDir = mkOption {
      default = acng-home + "/cache";
      type = types.str;
      description = ''
        Path to apt-cacher-ng cache directory.
        Will be created and chowned to acng-user
      '';
    };

    logDir = mkOption {
      default = acng-home + "/log";
      type = types.str;
      description = ''
        Path to apt-cacher-ng log directory.
        Will be created and chowned to acng-user
      '';
    };

    port = mkOption {
      default = 3142;
      type = types.int;
      description = ''
        port of apt-cacher-ng
      '';
    };

    bindAddress = mkOption {
      default = "";
      type = types.str;
      example = "localhost 192.168.7.254 publicNameOnMainInterface";
      description = ''
        listen address of apt-cacher-ng. Defaults to every interface.
      '';
    };

    cacheExpiration = mkOption {
      default = 4;
      type = types.int;
      description = ''
        number of days before packages expire in the cache without being
        requested.
      '';
    };

    enableNixCache = mkOption {
      default = true;
      type = types.bool;
      description = ''
        enable cache.nixos.org caching via PfilePatternEx and VfilePatternEx.

        to use the apt-cacher-ng in your nixos configuration:
          nix.binary-cache = [ http://acng-host:port/nixos ];

        These options cannot be used in extraConfig, use SVfilePattern and
        SPfilePattern or disable this option.
      '';
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      description = ''
        extra config appended to the generated acng.conf
      '';
    };
  };

  imp = {

    users.extraUsers.acng = {
      uid = genid "acng";
      description = "apt-cacher-ng";
      home = acng-home;
      createHome = false;
    };

    users.extraGroups.acng = {
      gid = genid "acng";
    };

    systemd.services.apt-cacher-ng = {
      description = "apt-cacher-ng";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        PermissionsStartOnly = true;
        ExecStartPre = pkgs.writeDash "acng-init" ''
          mkdir -p ${shell.escape cfg.cacheDir} ${shell.escape cfg.logDir}
          chown acng:acng  ${shell.escape cfg.cacheDir} ${shell.escape cfg.logDir}
        '';
        ExecStart = "${pkgs.apt-cacher-ng}/bin/apt-cacher-ng -c ${acng-config}";
        PrivateTmp = "true";
        User = "acng";
        Restart = "always";
        RestartSec = "10";
      };
    };
  };
in
{
  options.krebs.apt-cacher-ng = api;
  config = lib.mkIf cfg.enable imp;
}
