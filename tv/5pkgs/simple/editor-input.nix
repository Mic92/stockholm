{ pkgs }:
pkgs.writeDashBin "editor-input" ''
  exec \
  ${pkgs.utillinux}/bin/setsid -f \
  ${pkgs.with-tmpdir}/bin/with-tmpdir -t editor-input.XXXXXXXX \
  ${pkgs.writeDash "editor-input.sh" ''
    f=$TMPDIR/input
    ${pkgs.rxvt_unicode}/bin/urxvt -name editor-input-urxvt -e \
       ${pkgs.vim}/bin/vim --cmd ':set noeol binary' -c startinsert "$f"
    if test -e "$f"; then
      ${pkgs.xsel}/bin/xsel -ip < "$f"
      ${pkgs.xsel}/bin/xsel -ib < "$f"
      ${pkgs.xdotool}/bin/xdotool key --clearmodifiers shift+Insert
      ${pkgs.xsel}/bin/xsel -dp
      ${pkgs.xsel}/bin/xsel -db
    fi
  ''}
''
