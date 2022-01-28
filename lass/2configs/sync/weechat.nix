{
  services.syncthing.folders."/home/lass/.weechat".devices = [ "green" "mors" ];
  krebs.permown."/home/lass/.weechat" = {
    owner = "lass";
    group = "syncthing";
    umask = "0007";
  };
}
