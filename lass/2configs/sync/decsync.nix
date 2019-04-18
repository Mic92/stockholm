{
  krebs.syncthing.folders.decsync = {
    path = "/home/lass/decsync";
    peers = [ "mors" "blue" "green" "phone" ];
  };
  krebs.permown."/home/lass/decsync" = {
    owner = "lass";
    group = "syncthing";
    umask = "0007";
  };
}
