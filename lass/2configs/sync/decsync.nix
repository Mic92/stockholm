{
  krebs.syncthing.folders = [
    { id = "decsync"; path = "/home/lass/decsync"; peers = [ "mors" "blue" "green" "phone" ]; }
  ];
  lass.ensure-permissions = [
    { folder = "/home/lass/decsync"; owner = "lass"; group = "syncthing"; }
  ];
}
