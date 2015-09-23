{ config, pkgs, ... }:

#settings to test:
#
      #"ForceEphemeralProfiles": true,
let
  masterPolicy = pkgs.writeText "master.json" ''
    {
      "PasswordManagerEnabled": false,
      "DefaultGeolocationSetting": 2,
      "RestoreOnStartup": 1,
      "AutoFillEnabled": false,
      "BackgroundModeEnabled": false,
      "DefaultBrowserSettingEnabled": false,
      "SafeBrowsingEnabled": false,
      "ExtensionInstallForcelist": [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm;https://clients2.google.com/service/update2/crx",
        "ihlenndgcmojhcghmfjfneahoeklbjjh;https://clients2.google.com/service/update2/crx"
      ]
    }
  '';

  master_preferences = pkgs.writeText "master_preferences" ''
    {
      "browser": {
        "custom_chrome_frame": true
      },

      "extensions": {
        "theme": {
          "id": "",
          "use_system": true
        }
      }
    }
  '';
in {
  environment.etc."chromium/policies/managed/master.json".source = pkgs.lib.mkForce masterPolicy;

  #environment.systemPackages = [
  #  #pkgs.chromium
  #  (pkgs.lib.overrideDerivation pkgs.chromium (attrs: {
  #    buildCommand = attrs.buildCommand + ''
  #      touch $out/TEST123
  #    '';
  #  }))
  #];
}
