with import <stockholm/lib>;
self: super: {
  fzf = super.fzf.overrideAttrs (old: {
    # XXX cannot use `patches` because fzf has a custom patchPhase
    patchPhase = ''
      patch -Np1 < ${./fzf.complete1.patch}
      ${old.patchPhase or ""}
    '';
  });
  input-fonts = super.input-fonts.overrideAttrs (old: rec {
    src = self.fetchzip {
      url = "http://xu.r/~tv/mirrors/input-fonts/Input-Font-2.zip";
      sha256 = "1q58x92nm7dk9ylp09pvgj74nxkywvqny3xmfighnsl30dv42fcr";
      stripRoot = false;
    };
    sourceRoot = null;
    outputHash = null;
    outputHashAlgo = null;
    outputHashMode = null;
  });

}
//
mapNixDir (path: import path self super) ./.
