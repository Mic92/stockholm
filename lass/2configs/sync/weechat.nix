{
  services.syncthing.declarative.folders."/home/lass/.weechat".devices = [ "blue" "green" "mors" ];
  krebs.permown."/home/lass/.weechat" = {
    owner = "lass";
    group = "syncthing";
    umask = "0007";
  };
}
