{ pkgs }:

pkgs.flameshot-once.override {
  name = "flameshot-once-tv";
  config.imgur.enable = true;
  config.imgur.createUrl = "http://ni.r/image";
  config.imgur.deleteUrl = "http://ni.r/image/delete/%1";
  config.imgur.xdg-open.browser = "/etc/profiles/per-user/tv/bin/cr";
  config.settings.General = {
    autoCloseIdleDaemon = true;
    buttons = [
      "TYPE_ARROW"
      "TYPE_CIRCLE"
      "TYPE_CIRCLECOUNT"
      "TYPE_COPY"
      "TYPE_DRAWER"
      "TYPE_IMAGEUPLOADER"
      "TYPE_MARKER"
      "TYPE_MOVESELECTION"
      "TYPE_PENCIL"
      "TYPE_PIXELATE"
      "TYPE_RECTANGLE"
      "TYPE_SAVE"
      "TYPE_SELECTION"
      "TYPE_TEXT"
    ];
    checkForUpdates = false;
    contrastOpacity = 220;
    copyPathAfterSave = true;
    disabledTrayIcon = true;
    drawColor = "#E4002B";
    drawThickness = 8;
    filenamePattern = "%FT%T%z_flameshot";
    fontFamily = "iosevka tv 2";
    savePath = "/tmp";
    savePathFixed = true;
    showDesktopNotification = false;
    showHelp = false;
    showSidePanelButton = false;
    showStartupLaunchMessage = false;
    squareMagnifier = true;
    uploadWithoutConfirmation = true;
  };
  config.settings.Shortcuts = {
    TYPE_COPY = "Return";
    TYPE_TOGGLE_PANEL = "`";
  };
}
