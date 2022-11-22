{ config, lib, ... }: {
  config = lib.mkMerge (map (path: { krebs = import path { inherit config; }; }) [
    ./dbalan
    ./jeschli
    ./kmein
    ./krebs
    ./lass
    ./makefu
    ./mic92
    ./others
    ./palo
    ./rtunreal
    ./tv
  ]);
}
