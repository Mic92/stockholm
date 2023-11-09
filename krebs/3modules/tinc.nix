{ config, pkgs, lib, ... }:
with import ../../lib/pure.nix { inherit lib; }; {
  options.krebs.tinc = mkOption {
    default = {};
    description = ''
      define a tinc network
    '';
    type = types.attrsOf (types.submodule (tinc: {
      options = let
        netname = tinc.config._module.args.name;
      in {

        enable = mkEnableOption "krebs.tinc.${netname}" // { default = true; };

        confDir = mkOption {
          type = types.package;
          default = pkgs.linkFarm "${netname}-etc-tinc"
            (mapAttrsToList (name: path: { inherit name path; }) {
              "hosts" = tinc.config.hostsPackage;
              "tinc.conf" = pkgs.writeText "${netname}-tinc.conf" ''
                Name = ${tinc.config.host.name}
                LogLevel = ${toString tinc.config.logLevel}
                Interface = ${netname}
                Broadcast = no
                ${concatMapStrings (c: "ConnectTo = ${c}\n") tinc.config.connectTo}
                Port = ${toString tinc.config.host.nets.${netname}.tinc.port}
                ${tinc.config.extraConfig}
              '';
              "tinc-up" = pkgs.writeDash "${netname}-tinc-up" tinc.config.tincUp;
            });
        };

        host = mkOption {
          type = types.host;
          default = config.krebs.build.host;
        };

        netname = mkOption {
          type = types.enum (attrNames tinc.config.host.nets);
          default = netname;
          description = ''
            The tinc network name.
            It is used to name the TUN device and to generate the default value for
            <literal>config.krebs.tinc.retiolum.hosts</literal>.
          '';
        };

        extraConfig = mkOption {
          type = types.lines;
          default = "";
          description = ''
            Extra Configuration to be appended to tinc.conf
          '';
        };
        tincUp = mkOption {
          type = types.str;
          default = let
            net = tinc.config.host.nets.${netname};
            iproute = tinc.config.iproutePackage;
          in /* sh */ ''
            ${tinc.config.iproutePackage}/sbin/ip link set ${netname} up
            ${optionalString (net.ip4 != null) /* sh */ ''
              ${iproute}/sbin/ip -4 addr add ${net.ip4.addr} dev ${netname}
              ${iproute}/sbin/ip -4 route add ${net.ip4.prefix} dev ${netname}
            ''}
            ${optionalString (net.ip6 != null) /* sh */ ''
              ${iproute}/sbin/ip -6 addr add ${net.ip6.addr} dev ${netname}
              ${iproute}/sbin/ip -6 route add ${net.ip6.prefix} dev ${netname}
            ''}
          '';
          defaultText = /* sh */ ''
            ip link set ‹netname› up
            ip -4 addr add ‹net.ip4.addr› dev ‹netname›
            ip -4 route add ‹net.ip4.prefix› dev ‹netname›
            ip -6 addr add ‹net.ip6.addr› dev ‹netname›
            ip -6 route add ‹net.ip6.prefix› dev ‹netname›
          '';
          description = ''
            tinc-up script to be used. Defaults to setting the
            krebs.host.nets.‹netname›.ip4 and ip6 for the new ips and
            configures forwarding of the respecitive netmask as subnet.
          '';
        };

        tincPackage = mkOption {
          type = types.package;
          default = pkgs.tinc_pre;
          description = "Tincd package to use.";
        };

        hosts = mkOption {
          type = with types; attrsOf host;
          default =
            filterAttrs (_: h: hasAttr tinc.config.netname h.nets) config.krebs.hosts;
          defaultText = "‹all-hosts-of-‹netname››";
          description = ''
            Hosts to generate <literal>config.krebs.tinc.retiolum.hostsPackage</literal>.
            Note that these hosts must have a network named
            <literal>config.krebs.tinc.retiolum.netname</literal>.
          '';
        };

        hostsArchive = mkOption {
          type = types.package;
          default = pkgs.runCommand "retiolum-hosts.tar.bz2" {
            nativeBuildInputs = [ pkgs.gnutar pkgs.coreutils ];
          } ''
            cp \
                --no-preserve=mode \
                --recursive \
                ${tinc.config.hostsPackage} \
                hosts
            tar -cjf $out hosts
          '';
          readOnly = true;
        };

        hostsPackage = mkOption {
          type = types.package;
          default =
            pkgs.write "${tinc.config.netname}-tinc-hosts"
              (mapAttrs'
                (_: host: nameValuePair "/${host.name}" {
                  text = host.nets.${tinc.config.netname}.tinc.config;
                })
                tinc.config.hosts);
          defaultText = "‹netname›-tinc-hosts";
          description = ''
            Package of tinc host configuration files.  By default, a package will
            be generated from <literal>config.krebs.‹netname›.hosts</literal>.  This
            option's main purpose is to expose the generated hosts package to other
            modules, like <literal>config.krebs.tinc_graphs</literal>.  But it can
            also be used to provide a custom hosts directory.
          '';
          example = literalExample ''
            (pkgs.stdenv.mkDerivation {
              name = "my-tinc-hosts";
              src = /home/tv/my-tinc-hosts;
              installPhase = "cp -R . $out";
            })
          '';
        };

        iproutePackage = mkOption {
          type = types.package;
          default = pkgs.iproute2;
          description = "Iproute2 package to use.";
        };

        privkey = mkOption {
          type = types.absolute-pathname;
          default = "${config.krebs.secret.directory}/${tinc.config.netname}.rsa_key.priv";
          defaultText = "‹secrets/‹netname›.rsa_key.priv›";
        };

        privkey_ed25519 = mkOption {
          type = types.nullOr types.absolute-pathname;
          default =
            if tinc.config.host.nets.${netname}.tinc.pubkey_ed25519 == null
              then null
              else "${config.krebs.secret.directory}/${tinc.config.netname}.ed25519_key.priv";
          defaultText = "‹secrets/‹netname›.ed25519_key.priv›";
        };

        connectTo = mkOption {
          type = types.listOf types.str;
          ${if netname == "retiolum" then "default" else null} = [
            "eve"
            "ni"
            "prism"
          ];
          description = ''
            The list of hosts in the network which the client will try to connect
            to.  These hosts should have an 'Address' configured which points to a
            routeable IPv4 or IPv6 address.

            In stockholm this can be done by configuring:
            {
              krebs.hosts.‹host›.nets.‹netname›.via.ip4.addr = external-ip;
              krebs.hosts.‹host›.nets.‹netname›.tinc.port = 1655;
            }
          '';
        };

        logLevel = mkOption {
          type = types.int;
          description = ''
            LogLevel in tinc.conf
          '';
          default = 3;
        };

        username = mkOption {
          type = types.username;
          default = tinc.config.netname;
          defaultText = literalExample "netname";
        };
      };
    }));
  };

  config = {
    krebs.systemd.services = mapAttrs (netname: cfg: {
      restartIfCredentialsChange = true;
    }) config.krebs.tinc;

    systemd.services = mapAttrs (netname: cfg: {
      description = "Tinc daemon for ${netname}";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      reloadIfChanged = true;
      serviceConfig = {
        ExecReload = "+${cfg.tincPackage}/sbin/tinc -n ${netname} reload";
        Restart = "always";
        LoadCredential = filter (x: x != "") [
          (optionalString (cfg.privkey_ed25519 != null)
            "ed25519_key.priv:${cfg.privkey_ed25519}"
          )
          "rsa_key.priv:${cfg.privkey}"
        ];
        ExecStartPre = "+" + pkgs.writers.writeDash "init-tinc-${netname}" ''
          set -efu
          ${pkgs.coreutils}/bin/mkdir -p /etc/tinc
          ${pkgs.rsync}/bin/rsync -Lacv --delete \
            --chown ${cfg.username} \
            --chmod u=rwX,g=rX \
            --exclude='/*.priv' \
            ${cfg.confDir}/ /etc/tinc/${netname}/
          ${optionalString (cfg.privkey_ed25519 != null) /* sh */ ''
            ${pkgs.coreutils}/bin/ln -fns \
                "$CREDENTIALS_DIRECTORY"/ed25519_key.priv \
                /etc/tinc/${netname}/
          ''}
          ${pkgs.coreutils}/bin/ln -fns \
              "$CREDENTIALS_DIRECTORY"/rsa_key.priv \
              /etc/tinc/${netname}/
        '';
        ExecStart = "+" + toString [
          "${cfg.tincPackage}/sbin/tincd"
          "-D"
          "-U ${cfg.username}"
          "-d 0"
          "-n ${netname}"
        ];
        SyslogIdentifier = netname;
        DynamicUser = true;
        User = cfg.username;
      };
    }) config.krebs.tinc;
  };
}
