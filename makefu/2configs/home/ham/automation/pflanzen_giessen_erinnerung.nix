let
  notify_felix = message: {
    service = "notify.signal_felix";
    data.message = message;
  };
  notify_home = message: {
    service = "notify.signal_home";
    data.message = message;
  };
in
{
  services.home-assistant.config.automation =
  [
    {
      alias = "Pflanzen Giessen Erinnerung Daily";
      trigger = {
        platform = "time";
        at = "12:15:00";
      };
      action = [
        (notify_felix "Es ist Mittagszeit und du kannst ruhig einmal alle Blumen im Zimmer giessen")
      ];
    }
    {
      alias = "Pflanzen Giessen Erinnerung Weekly";
      trigger = {
        platform = "time";
        at = "12:15:00";
      };
      condition = {
        condition = "time";
        weekday = [ "sat" ];
      };
      action = [
        (notify_home "Es ist Wochenende und die Pflanzen würden sich über ein bisschen Wasser freuen.")
      ];
    }
  ];
}