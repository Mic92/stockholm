{
  services.syncthing.declarative.folders."/home/lass/sync" = {
    devices = [ "mors" "icarus" "xerxes" "shodan" "green" "blue" "coaxmetal" ];
  };
  krebs.permown."/home/lass/sync" = {
    file-mode = "u+rw,g+rw";
    owner = "lass";
    group = "syncthing";
    umask = "0002";
    keepGoing = true;
  };
}

