{
  krebs.syncthing.folders = [
    { path = "/home/lass/.weechat"; peers = [ "blue" "green" "mors" ]; }
  ];
  lass.ensure-permissions = [
    { folder = "/home/lass/.weechat"; owner = "lass"; group = "syncthing"; }
  ];
}
