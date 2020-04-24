{ config, pkgs, ... }:

let

  wrapper = pkgs.callPackage ../5pkgs/firefox/firefox-with-config.nix { };
  myFirefox = wrapper pkgs.firefox-unwrapped {

  # these plugins are defined in 5pkgs/firefox
  extraExtensions = with pkgs ; [
    dark-reader
    https-everywhere
    ublock-origin
    audio-fingerprint-defender
    canvas-fingerprint-defender
    webgl-fingerprint-defender
    font-fingerprint-defender
    user-agent-switcher
  ];

  extraPolicies = {
    CaptivePortal = false;
  };

  disablePocket = true;
  disableFirefoxSync = true;
  allowNonSigned = true;
  clearDataOnShutdown = true;
  disableDrmPlugin = true;

};

in {


environment.variables = {
  BROWSER = ["firefox"];
};


environment.systemPackages = with pkgs; [
  myFirefox
];

}
