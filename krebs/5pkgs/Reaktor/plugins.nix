{ stdenv, lib, pkgs, makeWrapper }:

rec {
  buildReaktorPlugin = { name
                        # TODO: profiles
                        , extraConfig
                        , phases ? []
                        , ... } @ attrs:
    stdenv.mkDerivation (attrs // {
      name = "Reaktor-plugin-" + name;
      phases = phases ++ [ "installPhase" ];
      isReaktorPlugin = true;
    });

  random-emoji = buildReaktorPlugin rec {
    name = "random-emoji";
    src = ./scripts/random-emoji.sh;
    phases = [ "installPhase" ];
    buildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      install -vm 755 ${src} $out/bin/random-emoji.sh
      wrapProgram $out/bin/random-emoji.sh \
        --prefix PATH : ${lib.makeSearchPath "bin" (with pkgs; [
                          coreutils
                          gnused
                          gnugrep
                          xmlstarlet
                          curl])};
    '';
    extraConfig = ''
      public_commands.insert(0,{
        'capname' : "emoji",
        'pattern' : indirect_pattern.format("emoji"),
        'argv'    : ["random-emoji.sh"])
    '';
  };
}
