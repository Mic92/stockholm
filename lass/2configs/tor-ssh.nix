{
  services.tor = {
    enable = true;
    relay.onionServices.ssh = {
      version = 3;
      map = [{
        port = 22;
        target.port = 22;
      }];
      secretKey = <secrets/ssh-tor.priv>;
    };
    controlSocket.enable = true;
    client.enable = true;
  };
}

