{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  pkg = pkgs.pulseaudioLight;
  runDir = "/run/pulse";

  alsaConf = pkgs.writeText "asound.conf" ''
    ctl_type.pulse {
      libs.native = ${pkgs.alsaPlugins}/lib/alsa-lib/libasound_module_ctl_pulse.so;
    }
    pcm_type.pulse {
      libs.native = ${pkgs.alsaPlugins}/lib/alsa-lib/libasound_module_pcm_pulse.so;
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

  daemonConf = pkgs.writeText "daemon.conf" ''
    exit-idle-time=0
    flat-volumes = no
    default-fragments = 4
    default-fragment-size-msec = 25
  '';

  configFile = pkgs.writeText "default.pa" ''
    .include ${pkg}/etc/pulse/default.pa
    load-module ${toString [
      "module-native-protocol-unix"
      "auth-anonymous=1"
      "socket=${runDir}/socket"
    ]}
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
      "pulse/daemon.pa".source = daemonConf;
    };
    systemPackages = [
      pkg
    ] ++ optionals config.services.xserver.enable [
      pkgs.pavucontrol
    ];
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
      ExecStart = "${pkg}/bin/pulseaudio";
      ExecStartPre = pkgs.writeDash "pulse-start" ''
        install -o pulse -g audio -m 0750 -d ${runDir}
        install -o pulse -g audio -m 0700 -d ${runDir}/home
      '';
      PermissionsStartOnly = "true";
      User = "pulse";
    };
  };

  users = {
    groups.pulse.gid = config.users.users.pulse.uid;
    users.pulse = {
      uid = genid "pulse";
      group = "pulse";
      extraGroups = [ "audio" ];
      home = "${runDir}/home";
    };
  };
}
