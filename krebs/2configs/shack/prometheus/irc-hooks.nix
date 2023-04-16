{ config
, lib
, pkgs
, ...
}:
let
  irc-alerts = pkgs.writers.writePython3 "irc-alerts" { 
    flakeIgnore = [ "E501" ];
  } (builtins.readFile ./irc-alerts.py);
  endpoints = {
    binaergewitter = {
      url = "irc+tls://puyak-alerts@irc.libera.chat:6697/#binaergewitter-alerts";
      port = 9223;
    };
  };
in
{
  systemd.sockets =
    lib.mapAttrs'
      (name: opts:
        lib.nameValuePair "irc-alerts-${name}" {
          description = "Receive http hook and send irc message for ${name}";
          wantedBy = [ "sockets.target" ];
          listenStreams = [ "[::]:${builtins.toString opts.port}" ];
        }) endpoints;

  systemd.services =
    lib.mapAttrs'
      (name: opts:
        let
          serviceName = "irc-alerts-${name}";
          hasPassword = opts.passwordFile or null != null;
        in
        lib.nameValuePair serviceName {
          description = "Receive http hook and send irc message for ${name}";
          requires = [ "irc-alerts-${name}.socket" ];
          serviceConfig =
            {
              Environment =
                [
                  "IRC_URL=${opts.url}"
                  "DEBUG=y"
                ]
                ++ lib.optional hasPassword "IRC_PASSWORD_FILE=/run/${serviceName}/password";
              DynamicUser = true;
              User = serviceName;
              ExecStart = irc-alerts;
            }
            // lib.optionalAttrs hasPassword {
              PermissionsStartOnly = true;
              ExecStartPre =
                "${pkgs.coreutils}/bin/install -m400 "
                + "-o ${serviceName} -g ${serviceName} "
                + "${config.sops.secrets.prometheus-irc-password.path} "
                + "/run/${serviceName}/password";
              RuntimeDirectory = serviceName;
            };
        }) endpoints;
}
