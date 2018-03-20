{ config, ... }:
with import <stockholm/lib>;
let
  nixos-version-id = "${config.system.nixosVersion}";
  nixos-version = "${nixos-version-id} (${config.system.nixosCodeName})";
  nixos-pretty-name = "NixOS ${nixos-version}";

  stockholm-version-id = let
    eval = tryEval (removeSuffix "\n" (readFile <stockholm-version>));
  in
    if eval.success then eval.value else "unknown";

  stockholm-version = "${stockholm-version-id}";
  stockholm-pretty-name = "stockholm ${stockholm-version}";

  version = "${stockholm-version}/${nixos-version}";
  version-id = "${stockholm-version-id}/${nixos-version-id}";
  pretty-name = "${stockholm-pretty-name} / ${nixos-pretty-name}";

  home-url = http://cgit.ni.krebsco.de/stockholm;
in
{
  # http://0pointer.de/public/systemd-man/os-release.html
  environment.etc."os-release".text = mkForce ''
    NAME="stockholm/NixOS"
    ID=stockholm
    VERSION="${version}"
    VERSION_ID="${version-id}"
    PRETTY_NAME="${pretty-name}"
    HOME_URL="${home-url}"
  '';
}
