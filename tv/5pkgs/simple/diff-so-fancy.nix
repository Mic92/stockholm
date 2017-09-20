{stdenv, git, perl, ncurses, coreutils, fetchFromGitHub, makeWrapper, ...}:

stdenv.mkDerivation rec {
  name = "diff-so-fancy-${version}";
  version = "ed8cf17";

  src = fetchFromGitHub {
    owner = "so-fancy";
    repo = "diff-so-fancy";
    rev = "ed8cf1763d38bdd79ceb55a73b9ce7e30f1e184d";
    sha256 = "176qn0w2rn6mr5ymvkblyiznqq7yyibfsnnjfivcyhz69w6yr9r9";
  };

  # Perl is needed here for patchShebangs
  nativeBuildInputs = [ perl makeWrapper ];

  buildPhase = null;

  installPhase = ''
    mkdir -p $out/bin $out/lib/diff-so-fancy

    # diff-so-fancy executable searches for it's library relative to
    # itself, so we are copying executable to lib, and only symlink it
    # from bin/
    cp diff-so-fancy $out/lib/diff-so-fancy
    cp -r lib $out/lib/diff-so-fancy
    ln -s $out/lib/diff-so-fancy/diff-so-fancy $out/bin

    # ncurses is needed for `tput`
    wrapProgram $out/lib/diff-so-fancy/diff-so-fancy \
      --prefix PATH : "${git}/share/git/contrib/diff-highlight" \
      --prefix PATH : "${git}/bin" \
      --prefix PATH : "${coreutils}/bin" \
      --prefix PATH : "${ncurses.out}/bin"
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/so-fancy/diff-so-fancy;
    description = "Good-looking diffs filter for git";
    license = licenses.mit;
    platforms = platforms.all;
    longDescription = ''
      diff-so-fancy builds on the good-lookin' output of git contrib's
      diff-highlight to upgrade your diffs' appearances.
    '';
    maintainers = with maintainers; [ fpletz ];
  };
}
