{
  services.syncthing.folders."/home/lass/sync" = {
    devices = [ "mors" "icarus" "xerxes" "shodan" "green" "blue" "coaxmetal" ];
  };
  krebs.acl."/home/lass/sync"."u:syncthing:X".parents = true;
  krebs.acl."/home/lass/sync"."u:syncthing:rwX" = {};
  krebs.acl."/home/lass/sync"."u:lass:rwX" = {};
}
