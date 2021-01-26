with import <stockholm/lib>;

{
  imports =
    map
      (name: ./. + "/${name}")
      (filter
        (name: name != "default.nix" && !hasPrefix "." name)
        (attrNames (readDir ./.)));
}
