{ ... }:

rec {
  Just = x: { type = "maybe"; value = x; };
  Nothing = { type = "maybe"; };
  isMaybe = x: builtins.typeOf x == "set" && x.type or false == "maybe";
  isJust = x: isMaybe x && builtins.hasAttr "value" x;
  fromJust = x: assert isJust x; x.value;
  catMaybes = xs: map fromJust (builtins.filter isJust xs);
}
