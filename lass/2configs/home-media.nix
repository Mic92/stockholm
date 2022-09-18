with import <stockholm/lib>;
{ pkgs, ... }:
{
  users.users.media = {
    isNormalUser = true;
    uid = genid_uint31 "media";
    extraGroups = [ "video" "audio" "pipewire" ];
  };

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "media";
  };

  hardware.pulseaudio.configFile = pkgs.writeText "pulse.pa" ''
    .include ${pkgs.pulseaudioFull}/etc/pulse/default.pa
    load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1;10.42.0.0/24 auth-anonymous=1
  '';

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 4713"; target = "ACCEPT"; } # pulseaudio
  ];

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "snapmpv" ''
      /run/current-system/sw/bin/mpv \
        --audio-display=no --audio-channels=stereo \
        --audio-samplerate=48000 --audio-format=s16 \
        --ao-pcm-file=/run/snapserver/snapfifo --ao=pcm \
        --audio-delay=-1 \
        "$@"
    '')
  ];
}
