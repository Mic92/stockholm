{
  services.syncthing.folders.decsync = {
    path = "/home/lass/decsync";
    devices = [ "mors" "blue" "green" "phone" ];
  };
  krebs.permown."/home/lass/decsync" = {
    owner = "lass";
    group = "syncthing";
    umask = "0007";
  };
}
