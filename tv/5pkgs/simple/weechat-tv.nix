{ lib, pkgs }:

pkgs.wrapWeechat pkgs.weechat-unwrapped {
  configure = { availablePlugins, ... }: {
    scripts = [
      pkgs.weechatScripts.weechat-matrix
    ];
  };
}
