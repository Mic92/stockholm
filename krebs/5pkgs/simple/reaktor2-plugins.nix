{ lib, pkgs, stockholm, ... }:
with stockholm.lib;

rec {
  generators = {
    command_hook = commands: {
      pattern =
        "^\\s*([0-9A-Za-z._][0-9A-Za-z._-]*)(?:\\s+(.*\\S))?\\s*$";
      command = 1;
      arguments = [2];
      commands = commands;
    };
  };

  commands = {

    random-emoji = {
      filename = <stockholm/krebs/5pkgs/simple/Reaktor/scripts/random-emoji.sh>;
      env = {
        PATH = makeBinPath (with pkgs; [ coreutils gnused gnugrep xmlstarlet wget ]);
      };
    };

    dance = {
      filename = pkgs.writeDash "dance" ''
        echo "<(^.^<)"
        echo "<(^.^)>"
        echo "(>^.^)>"
        echo "(7^.^)7"
        echo "(>^.^<)"
      '';
    };

    nixos-version = {
      filename = pkgs.writeDash "nixos-version" ''
        . /etc/os-release
        echo "$PRETTY_NAME"
      '';
    };

    stockholm-issue = {
      filename = <stockholm/krebs/5pkgs/simple/Reaktor/scripts/random-issue.sh>;
      env = {
        PATH = makeBinPath (with pkgs; [ coreutils git gnused haskellPackages.lentil ]);
        origin = "http://cgit.gum/stockholm";
        state_dir = "/tmp/stockholm-issue";
      };
    };

  };

  hooks = {

    sed = {
      activate = "always";
      pattern = "^(.*)$";
      arguments = [1];
      command = {
        env = {
          PATH = makeBinPath (with pkgs; [ gnused ]);
          state_dir = "/tmp";
        };
        filename = pkgs.writeDash "sed-plugin" ''
          set -efu
          exec ${pkgs.python3}/bin/python \
              ${<stockholm/krebs/5pkgs/simple/Reaktor/scripts/sed-plugin.py>} "$@"
        '';
      };
    };

    shack-correct = {
      activate = "match";
      pattern = "^(.*Shack.*)$";
      arguments = [1];
      command.filename = <stockholm/krebs/5pkgs/simple/Reaktor/scripts/shack-correct.sh>;
    };


    url-title = {
      #pattern = "^.*(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+).*$";
      pattern = "^.*(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+).*$";
      activate = "match";
      arguments = [1];
      command = {
        filename = pkgs.writePython3 "url-title" { deps = with pkgs.python3Packages; [ beautifulsoup4 lxml ]; } ''
          import cgi
          import sys
          import urllib.request
          from bs4 import BeautifulSoup

          try:
              req = urllib.request.Request(sys.argv[1])
              req.add_header('user-agent', 'Reaktor-url-title')
              resp = urllib.request.urlopen(req)
              if resp.headers['content-type'].find('text/html') >= 0:
                  soup = BeautifulSoup(resp.read(16000), "lxml")
                  title = soup.find('title').string

                  if len(title.split('\n')) > 5:
                      title = '\n'.join(title.split('\n')[:5])

                  print(title[:450])
              else:
                  cd_header = resp.headers['content-disposition']
                  print(cgi.parse_header(cd_header)[1]['filename'])
          except:  # noqa: E722
              pass
        '';
      };
    };
  };
}
