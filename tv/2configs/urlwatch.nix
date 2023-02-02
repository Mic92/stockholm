with import ./lib;
{ config, pkgs, ... }: let
  exec = filename: args: url: {
    inherit url;
    filter = singleton {
      system =
        concatMapStringsSep " " shell.escape ([filename] ++ toList args);
    };
  };
  json = json' ["."];
  json' = exec "${pkgs.jq}/bin/jq";
  xml = xml' ["--format" "-"];
  xml' = exec "${pkgs.libxml2}/bin/xmllint";
in {
  krebs.urlwatch = {
    enable = true;
    mailto = config.krebs.users.tv.mail;
    onCalendar = "*-*-* 05:00:00";
    urls = [
      ## nixpkgs maintenance

      # 2014-07-29 when one of the following urls change
      # then we have to update the package

      http://www.exim.org/

      # ref src/nixpkgs/pkgs/tools/networking/urlwatch/default.nix
      {
        url = https://thp.io/2008/urlwatch/;
        # workaround: ('Received response with content-encoding: gzip, but
        # failed to decode it.', error('Error -3 while decompressing data:
        # incorrect header check',))
        ignore_cached = true;
      }

      # 2015-02-18
      # ref ~/src/nixpkgs/pkgs/tools/text/qprint/default.nix
      http://www.fourmilab.ch/webtools/qprint/

      # 2014-09-24 ref https://github.com/4z3/xintmap
      http://www.mathstat.dal.ca/~selinger/quipper/

      ## 2014-10-17
      ## TODO update ~/src/login/default.nix
      #http://hackage.haskell.org/package/bcrypt
      #http://hackage.haskell.org/package/cron
      #http://hackage.haskell.org/package/hyphenation
      #http://hackage.haskell.org/package/iso8601-time
      #http://hackage.haskell.org/package/ixset-typed
      #http://hackage.haskell.org/package/system-command
      #http://hackage.haskell.org/package/transformers
      #http://hackage.haskell.org/package/web-routes-wai
      #http://hackage.haskell.org/package/web-page

      # ref <stockholm/krebs/3modules>, services.openssh.knownHosts.github*
      (json https://api.github.com/meta)

      # ref <nixpkgs/pkgs/tools/security/ssh-audit>
      (json https://api.github.com/repos/arthepsy/ssh-audit/tags)

      # 2014-12-20 ref src/nixpkgs/pkgs/tools/networking/tlsdate/default.nix
      (json https://api.github.com/repos/ioerror/tlsdate/tags)

      # ref src/nixpkgs/pkgs/tools/admin/sec/default.nix
      (json https://api.github.com/repos/simple-evcorr/sec/tags)

      # <stockholm/tv/2configs/xserver/xserver.conf.nix>
      # is derived from `configFile` in:
      https://raw.githubusercontent.com/NixOS/nixpkgs/master/nixos/modules/services/x11/xserver.nix

      https://www.rabbitmq.com/changelog.html
    ];
    hooksFile = toFile "hooks.py" ''
      import subprocess
      import urlwatch

      class SystemFilter(urlwatch.filters.FilterBase):
          """Filter for piping data through an external process"""

          __kind__ = 'system'

          __supported_subfilters__ = {
              'command': 'shell command line to tranform data',
          }

          __default_subfilter__ = 'command'

          def filter(self, data, subfilter=None):
              if 'command' not in subfilter:
                  raise ValueError('{} filter needs a command'.format(self.__kind__))

              proc = subprocess.Popen(
                  subfilter['command'],
                  shell=True,
                  stdin=subprocess.PIPE,
                  stdout=subprocess.PIPE,
                  stderr=subprocess.PIPE,
                  )

              (stdout, stderr) = proc.communicate(data.encode())

              if proc.returncode != 0:
                  raise RuntimeError(
                    "system filter returned non-zero exit status %d; stderr:\n"
                    % proc.returncode
                    + stderr.decode()
                    )

              return stdout.decode()
    '';
  };
}
