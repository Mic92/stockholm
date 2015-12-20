{ lib, ... }:

with lib;

{

  getDefaultGateway = ip:
    concatStringsSep "." (take 3 (splitString "." ip) ++ ["1"]);

}
