self: super:

super.fzf.overrideAttrs (old: {
  # XXX cannot use `patches` because fzf has a custom patchPhase
  patchPhase = ''
    patch -Np1 < ${./complete1.patch}
    ${old.patchPhase or ""}
  '';
})
