{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  pkg = pkgs.pulseaudio;
  runDir = "/run/pulse";

  pkgs_i686 = pkgs.pkgsi686Linux;

  support32Bit =
    pkgs.stdenv.isx86_64 &&
    pkgs_i686.alsaLib != null &&
    pkgs_i686.libpulseaudio != null;

  alsaConf = pkgs.writeText "asound.conf" ''
    ctl_type.pulse {
      libs.native = ${pkgs.alsaPlugins}/lib/alsa-lib/libasound_module_ctl_pulse.so;
      ${optionalString support32Bit
        "libs.32Bit = ${pkgs_i686.alsaPlugins}/lib/alsa-lib/libasound_module_ctl_pulse.so;"}
    }
    pcm_type.pulse {
      libs.native = ${pkgs.alsaPlugins}/lib/alsa-lib/libasound_module_pcm_pulse.so;
      ${optionalString support32Bit
        "libs.32Bit = ${pkgs_i686.alsaPlugins}/lib/alsa-lib/libasound_module_pcm_pulse.so;"}
    }
    ctl.!default {
      type pulse
    }
    pcm.!default {
      type pulse
    }
  '';

  clientConf = pkgs.writeText "client.conf" ''
    autospawn=no
    default-server = unix:${runDir}/socket
  '';

  configFile = pkgs.writeText "default.pa" ''
    .include ${pkg}/etc/pulse/default.pa
    load-module ${toString [
      "module-native-protocol-unix"
      "auth-anonymous=1"
      "socket=${runDir}/socket"
    ]}
    ${lib.optionalString (config.krebs.build.host.name == "au") ''
      load-module ${toString [
        "module-native-protocol-tcp"
        "auth-ip-acl=127.0.0.1;10.23.1.0/24"
      ]}
    ''}
    ${lib.optionalString (config.krebs.build.host.name != "au") ''
      load-module ${toString [
        "module-tunnel-sink-new"
        "server=au.hkw"
        "sink_name=au"
        "channels=2"
        "rate=44100"
      ]}
    ''}
  '';
in

{
  environment = {
    etc = {
      "asound.conf".source = alsaConf;
      # XXX mkForce is not strong enough (and neither is mkOverride) to create
      # /etc/pulse/client.conf, see pulseaudio-hack below for a solution.
      #"pulse/client.conf" = mkForce { source = clientConf; };
      #"pulse/client.conf".source = mkForce clientConf;
      "pulse/default.pa".source = configFile;
    };
    systemPackages = [
      pkg
    ] ++ optionals config.services.xserver.enable [
      pkgs.pavucontrol
    ];
  };

  hardware.pulseaudio = {
    inherit support32Bit;
  };

  # Allow PulseAudio to get realtime priority using rtkit.
  security.rtkit.enable = true;

  system.activationScripts.pulseaudio-hack = ''
    ln -fns ${clientConf} /etc/pulse/client.conf
  '';

  systemd.services.pulse = {
    wantedBy = [ "sound.target" ];
    before = [ "sound.target" ];
    environment = {
      PULSE_RUNTIME_PATH = "${runDir}/home";
    };
    serviceConfig = {
      ExecStart = "${pkg}/bin/pulseaudio --exit-idle-time=-1";
      ExecStartPre = pkgs.writeDash "pulse-start" ''
        install -o pulse -g pulse -m 0750 -d ${runDir}
        install -o pulse -g pulse -m 0700 -d ${runDir}/home
      '';
      PermissionsStartOnly = "true";
      User = "pulse";
    };
  };

  # TODO assert that pulse is the only user with "audio" in group/extraGroups
  # otherwise the audio device can be hijacked while the pulse service restarts
  # (e.g. when mpv is running) and then the service will fail.
  users = {
    groups.pulse.gid = config.users.users.pulse.uid;
    users.pulse = {
      uid = genid_uint31 "pulse";
      group = "pulse";
      extraGroups = [ "audio" ];
      home = "${runDir}/home";
      isSystemUser = true;
    };
  };
}
