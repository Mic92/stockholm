{ pkgs, ...}:
{
  systemd.services.alertmanager-bot-telegram = {
    wantedBy = [ "multi-user.target" ];
    after = [ "ip-up.target" ];
    serviceConfig = {
      EnvironmentFile = toString <secrets/shack/telegram_bot.env>;
      DynamicUser = true;
      StateDirectory = "alertbot";
      ExecStart = ''${pkgs.alertmanager-bot-telegram}/bin/alertmanager-bot \
        --alertmanager.url=http://alert.prometheus.shack --log.level=debug \
        --store=bolt --bolt.path=/var/lib/alertbot/bot.db \
        --listen.addr="0.0.0.0:16320" \
        --template.paths=${./templates}/shack.tmpl'';
    };
  };
}
