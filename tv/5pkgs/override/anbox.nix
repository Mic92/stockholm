self: super:

# https://github.com/NixOS/nixpkgs/pull/125600
super.anbox.overrideAttrs (old:
  assert old.version == "unstable-2020-11-29";
  rec {
    version = "unstable-2021-05-26";

    src = self.fetchFromGitHub {
      owner = old.pname;
      repo = old.pname;
      rev = "ad377ff25354d68b76e2b8da24a404850f8514c6";
      sha256 = "1bj07ixwbkli4ycjh41mnqdbsjz9haiwg2nhf9anbi29z1d0819w";
      fetchSubmodules = true;
    };

    postPatch = old.patchPhase;

    # patchPhase() from <nixpkgs/pkgs/stdenv/generic/setup.sh>
    # TODO patchPhase = default.patchPhase; or something
    patchPhase = ''
      runHook prePatch

      for i in ''${patches:-}; do
          header "applying patch $i" 3
          local uncompress=cat
          case "$i" in
              *.gz)
                  uncompress="gzip -d"
                  ;;
              *.bz2)
                  uncompress="bzip2 -d"
                  ;;
              *.xz)
                  uncompress="xz -d"
                  ;;
              *.lzma)
                  uncompress="lzma -d"
                  ;;
          esac
          # "2>&1" is a hack to make patch fail if the decompressor fails (nonexistent patch, etc.)
          # shellcheck disable=SC2086
          $uncompress < "$i" 2>&1 | patch ''${patchFlags:--p1}
      done

      runHook postPatch
    '';

    patches = [
      # Fixes compatibility with lxc 4
      (self.fetchpatch {
        url = "https://git.alpinelinux.org/aports/plain/community/anbox/lxc4.patch?id=64243590a16aee8d4e72061886fc1b15256492c3";
        sha256 = "1da5xyzyjza1g2q9nbxb4p3njj2sf3q71vkpvmmdphia5qnb0gk5";
      })
      # Wait 10× more time when starting
      # Not *strictly* needed, but helps a lot on slower hardware
      (self.fetchpatch {
        url = "https://git.alpinelinux.org/aports/plain/community/anbox/give-more-time-to-start.patch?id=058b56d4b332ef3379551b343bf31e0f2004321a";
        sha256 = "0iiz3c7fgfgl0dvx8sf5hv7a961xqnihwpz6j8r0ib9v8piwxh9a";
      })
      # Ensures generated desktop files work on store path change
      (self.fetchpatch {
        url = "https://raw.githubusercontent.com/NixOS/nixpkgs/fdf7b4be1a659ed8b96586c2fc8ff90850e25feb/pkgs/os-specific/linux/anbox/0001-NixOS-Use-anbox-from-PATH-in-desktop-files.patch";
        sha256 = "173z84g1r8h367a2dgk6945is6vxg4j1hs2hkfd3m3bbpwfw9221";
      })
      # Provide window icons
      (self.fetchpatch {
        url = "https://github.com/samueldr/anbox/commit/2387f4fcffc0e19e52e58fb6f8264fbe87aafe4d.patch";
        sha256 = "12lmr0kxw1n68g3abh1ak5awmpczfh75c26f53jc8qpvdvv1ywha";
      })
    ];
})
