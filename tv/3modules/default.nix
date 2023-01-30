with import ./lib;
{
  imports =
    map
      (name: ./. + "/${name}")
      (attrNames
        (filterAttrs isNixDirEntry (readDir ./.)));
}
