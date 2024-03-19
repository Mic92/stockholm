{ config, lib, ... }:
with lib;
let
  nixos-version-id = if (hasAttr "nixos" config.system) then
    "${config.system.nixos.version}" else "${config.system.nixosVersion}";
  nixos-codeName = if (hasAttr "nixos" config.system) then
    "${config.system.nixos.codeName}" else "${config.system.nixosCodeName}";
  nixos-version = "${nixos-version-id} (${nixos-codeName})";
  nixos-pretty-name = "NixOS ${nixos-version}";

  stockholm-version = "${config.krebs.os-release.stockholm-version-id}";
  stockholm-pretty-name = "stockholm ${stockholm-version}";

  version = "${stockholm-version}/${nixos-version}";
  version-id = "${config.krebs.os-release.stockholm-version-id}/${nixos-version-id}";
  pretty-name = "${stockholm-pretty-name} / ${nixos-pretty-name}";

  home-url = http://cgit.ni.krebsco.de/stockholm;
in
{
  options.krebs.os-release = {
    stockholm-version-id = lib.mkOption {
      type = lib.types.str;
      default = let
          eval = builtins.tryEval (removeSuffix "\n" (readFile <stockholm-version>));
        in
          if eval.success then eval.value else "unknown";
    };
  };
  config = {
    # http://0pointer.de/public/systemd-man/os-release.html
    environment.etc."os-release".text = mkForce ''
      NAME="stockholm/NixOS"
      ID=stockholm
      VERSION="${version}"
      VERSION_ID="${version-id}"
      PRETTY_NAME="${pretty-name}"
      HOME_URL="${home-url}"
    '';
  };
}
