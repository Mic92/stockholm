lib:
with lib;
mapNixDir (flip import lib) ./.
