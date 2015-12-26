{ config, lib, pkgs, ... }:

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
  systemd.tmpfiles.rules = [
    "d ${runDir} 0750 pulse pulse - -"
    "d ${runDir}/home 0700 pulse pulse - -"
  ];

  system.activationScripts.pulseaudio-hack = ''
    ln -fns ${clientConf} /etc/pulse/client.conf
  '';

  environment = {
    etc = {
      "asound.conf".source = alsaConf;
      #"pulse/client.conf" = lib.mkForce { source = clientConf; };
      "pulse/default.pa".source = configFile;
    };
    systemPackages = [ pkg ];
  };

  # Allow PulseAudio to get realtime priority using rtkit.
  security.rtkit.enable = true;

  systemd.services.pulse = {
    wantedBy = [ "sound.target" ];
    before = [ "sound.target" ];
    environment = {
      PULSE_RUNTIME_PATH = "${runDir}/home";
      #DISPLAY = ":${toString config.services.xserver.display}";
    };
    serviceConfig = {
      ExecStart = "${pkg}/bin/pulseaudio";
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
