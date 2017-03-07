{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  krebs.urlwatch = {
    enable = true;
    mailto = config.krebs.users.tv.mail;
    onCalendar = "*-*-* 05:00:00";
    urls = [
      ## nixpkgs maintenance

      # 2014-07-29 when one of the following urls change
      # then we have to update the package

      http://www.exim.org/

      # ref src/nixpkgs/pkgs/tools/admin/sec/default.nix
      https://api.github.com/repos/simple-evcorr/sec/tags

      # ref src/nixpkgs/pkgs/tools/networking/urlwatch/default.nix
      https://thp.io/2008/urlwatch/

      # 2014-12-20 ref src/nixpkgs/pkgs/tools/networking/tlsdate/default.nix
      https://api.github.com/repos/ioerror/tlsdate/tags

      # 2015-02-18
      # ref ~/src/nixpkgs/pkgs/tools/text/qprint/default.nix
      http://www.fourmilab.ch/webtools/qprint/

      # 2014-09-24 ref https://github.com/4z3/xintmap
      http://www.mathstat.dal.ca/~selinger/quipper/

      ## other

      https://nixos.org/channels/nixos-17.03/git-revision
      https://nixos.org/channels/nixos-unstable/git-revision

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
      https://help.github.com/articles/github-s-ip-addresses/

      # <stockholm/tv/2configs/xserver/xserver.conf.nix>
      # is derived from `configFile` in:
      https://raw.githubusercontent.com/NixOS/nixpkgs/master/nixos/modules/services/x11/xserver.nix
    ];
    hooksFile = toFile "hooks.py" ''
      import subprocess
      import urlwatch

      class CaseFilter(urlwatch.filters.FilterBase):
          """Filter for piping data through an external process"""

          __kind__ = 'system'

          def filter(self, data, subfilter=None):
              if subfilter is None:
                  raise ValueError('The system filter needs a command')

              proc = subprocess.Popen(
                  subfilter,
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
