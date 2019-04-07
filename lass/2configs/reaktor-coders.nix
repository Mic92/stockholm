{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

let
  hooks = pkgs.reaktor2-plugins.hooks;
in {
  krebs.reaktor2.coders = {
    hostname = "irc.hackint.org";
    port = "9999";
    useTLS = true;
    nick = "reaktor2|lass";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#coders"
            "#germany"
            "#panthermoderns"
          ];
        };
      }
      {
        plugin = "system";
        config = {
          workdir = config.krebs.reaktor2.coders.stateDir;
          hooks.PRIVMSG = [
            hooks.sed
            hooks.url-title
            {
              activate = "match";
              pattern = ''^@([^ ]+) (.*)$'';
              command = 1;
              arguments = [2];
              env.HOME = config.krebs.reaktor2.coders.stateDir;
              commands = let
                lambdabot = (import (pkgs.fetchFromGitHub {
                  owner = "NixOS"; repo = "nixpkgs";
                  rev = "a4ec1841da14fc98c5c35cc72242c23bb698d4ac";
                  sha256 = "148fpw31s922hxrf28yhrci296f7c7zd81hf0k6zs05rq0i3szgy";
                }) {}).lambdabot;
                lambdabotWrapper = pkgs.writeDash "lambdabot.wrapper" ''
                  exec ${lambdabot}/bin/lambdabot \
                    -XStandaloneDeriving -XGADTs -XFlexibleContexts \
                    -XFlexibleInstances -XMultiParamTypeClasses \
                    -XOverloadedStrings -XFunctionalDependencies \
                    -e "$@"
                '';
              in {
                pl.filename = pkgs.writeDash "lambdabot-pl" ''
                  ${lambdabotWrapper} "@pl $1"
                '';
                type.filename = pkgs.writeDash "lambdabot-type" ''
                  ${lambdabotWrapper} "@type $1"
                '';
                "let".filename = pkgs.writeDash "lambdabot-let" ''
                  ${lambdabotWrapper} "@let $1"
                '';
                run.filename = pkgs.writeDash "lambdabot-run" ''
                  ${lambdabotWrapper} "@run $1"
                '';
                kind.filename = pkgs.writeDash "lambdabot-kind" ''
                  ${lambdabotWrapper} "@kind $1"
                '';
              };
            }
            {
              activate = "match";
              pattern = ''^!([^ ]+)(?:\s*(.*))?'';
              command = 1;
              arguments = [2];
              commands = {
                ping.filename = pkgs.writeDash "ping" ''
                  exec /run/wrappers/bin/ping -q -c1 "$1" 2>&1 | tail -1
                '';
                google.filename = pkgs.writeDash "google" ''
                  exec ${pkgs.ddgr}/bin/ddgr -C -n1 --json "$@" | \
                    ${pkgs.jq}/bin/jq '@text "\(.[0].abstract) \(.[0].url)"'
                '';
                shrug.filename = pkgs.writeDash "shrug" ''
                  exec echo '¯\_(ツ)_/¯'
                '';
                table.filename = pkgs.writeDash "table" ''
                  exec echo '(╯°□°）╯ ┻━┻'
                '';
              };
            }
          ];
        };
      }
    ];
  };
}
