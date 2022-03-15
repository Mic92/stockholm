{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  options.lass.nichtparasoup = {
    enable = mkEnableOption "nichtparasoup funny image page";
    config = mkOption {
      type = types.str;
      default = ''
        [General]
        Port: 5001
        IP: 0.0.0.0
        Useragent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10) AppleWebKit/600.1.25 (KHTML, like Gecko) Version/8.0 Safari/600.1.25

        [Cache]
        Images_min_limit: 15

        [Logging]
        ;; possible destinations: file syslog
        Destination: syslog
        Verbosity: ERROR

        [Sites]
        SoupIO: everyone
        Pr0gramm: new,top
        Reddit: ${lib.concatStringsSep "," [
          "ANormalDayInRussia"
          "EngineeringPorn"
          "OSHA"
          "PeopleFuckingDying"
          "PerfectTiming"
          "PixelArt"
          "RetroFuturism"
          "ThingsCutInHalfPorn"
          "Unexpected"
          "abandonedporn"
          "animalsbeingderps"
          "assholedesign"
          "bizarrebuildings"
          "bonehurtingjuice"
          "boottoobig"
          "bossfight"
          "buddhistmemes"
          "cablefail"
          "cableporn"
          "catastrophicfailure"
          "chairsunderwater"
          "clevercomebacks"
          "crappydesign"
          "cursedcomments"
          "desirepath"
          "doenerverbrechen"
          "dontdeadopeninside"
          "educationalgifs"
          "eyebleach"
          "forbiddensnacks"
          "funnyanimals"
          "gifs"
          "grssk"
          "hmm"
          "holdmybeer"
          "holup"
          "iamatotalpieceofshit"
          "ichbin40undlustig"
          "idiotsincars"
          "illegallysmolcats"
          "instagramreality"
          "instant_regret"
          "itsaunixsystem"
          "kamikazebywords"
          "keming"
          "kidsarefuckingstupid"
          "kitchenconfidential"
          "laughingbuddha"
          "loadingicon"
          "michaelbaygifs"
          "mildlyinfuriating"
          "natureisfuckinglit"
          "nononoyesno"
          "notinteresting"
          "notliketheothergirls"
          "oddlysatisfying"
          "ofcoursethatsathing"
          "perfectloops"
          "picsofunusualbirds"
          "pizzacrimes"
          "prequelmemes"
          "reactiongifs"
          "reallifedoodles"
          "robotsbeingjerks"
          "scriptedasiangifs"
          "shitposting"
          "shittyfoodporn"
          "shittyrobots"
          "softwaregore"
          "startledcats"
          "startrekstabilized"
          "stupidfood"
          "techsupportgore"
          "thathappened"
          "totallynotrobots"
          "trippinthroughtime"
          "unexpected"
          "wasletztepreis"
          "wellthatsucks"
          "wewantplates"
          "whatcouldgowrong"
          "whatsthisbug"
          "whatsthisplant"
          "whatswrongwithyourdog"
          "whenthe"
          "yesyesyesyesno"
          "youseeingthisshit"
        ]}
        NineGag: geeky,wtf,hot,trending
        Instagram: nature,wtf
        Fourchan: sci
      '';
    };
  };

  config = mkIf config.lass.nichtparasoup.enable {
    systemd.services.nichtparasoup = {
      description = "nichtparasoup";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;
      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.nichtparasoup}/bin/nichtparasoup -c ${pkgs.writeText "config.ini"config.lass.nichtparasoup.config}";
      };
    };
  };
}
