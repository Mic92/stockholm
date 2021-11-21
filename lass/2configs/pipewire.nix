{ config, lib, pkgs, ... }:
# TODO test `alsactl init` after suspend to reinit mic
{
  security.rtkit.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # autostart with login
  systemd.user.services.pipewire-pulse = {
    wantedBy = [ "graphical-session.target" ];
  };

  environment.systemPackages = with pkgs; [
    alsaUtils
    pulseaudioLight
    ponymix
  ];

  environment.variables.PULSE_SERVER = "localhost:4713";
  services.pipewire = {
    enable = true;
    socketActivation = false;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    # https://gitlab.freedesktop.org/pipewire/pipewire/-/wikis/Migrate-PulseAudio#module-native-protocol-tcp
    config.pipewire-pulse = {
      "context.properties" = {
        "log.level" = 2;
      };
      "context.modules" = [
        {
          name = "libpipewire-module-rtkit";
          # args = {
          #   "nice.level" = -15;
          #   "rt.prio" = 88;
          #   "rt.time.soft" = 200000;
          #   "rt.time.hard" = 200000;
          # };
          flags = [ "ifexists" "nofail" ];
        }
        { name = "libpipewire-module-protocol-native"; }
        { name = "libpipewire-module-client-node"; }
        { name = "libpipewire-module-adapter"; }
        { name = "libpipewire-module-metadata"; }
        {
          name = "libpipewire-module-protocol-pulse";
          args = {
            "vm.overrides" = {
              # "pulse.min.req" = "32/48000";
              # "pulse.default.req" = "32/48000";
              # "pulse.max.req" = "32/48000";
              "pulse.min.quantum" = "1024/48000";
              # "pulse.max.quantum" = "32/48000";
            };
            "server.address" = [
              "unix:native"
              "tcp:4713"
            ];
          };
        }
      ];
      "stream.properties" = {
        # "node.latency" = "32/48000";
        # "resample.quality" = 1;
      };
    };
  };
}
