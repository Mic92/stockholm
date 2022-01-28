{
  services.syncthing.folders.the_playlist = {
    path = "/home/lass/tmp/the_playlist";
    devices = [ "mors" "phone" "prism" ];
  };
  lass.acl."/home/lass/tmp/the_playlist"."u:syncthing:X".parents = true;
  lass.acl."/home/lass/tmp/the_playlist"."u:syncthing:rwX" = {};
  lass.acl."/home/lass/tmp/the_playlist"."u:lass:rwX" = {};
}
