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
        Reddit: gifs,reactiongifs,ANormalDayInRussia,perfectloops,reallifedoodles,bizarrebuildings,cablefail,cableporn,educationalgifs,EngineeringPorn,holdmybeer,itsaunixsystem,loadingicon,michaelbaygifs,nononoyesno,oddlysatisfying,ofcoursethatsathing,OSHA,PeopleFuckingDying,PerfectTiming,PixelArt,RetroFuturism,robotsbeingjerks,scriptedasiangifs,shittyrobots,startrekstabilized,ThingsCutInHalfPorn,totallynotrobots,Unexpected,abandonedporn,animalsbeingderps,assholedesign,bonehurtingjuice,boottoobig,bossfight,buddhistmemes,catastrophicfailure,chairsunderwater,clevercomebacks,crappydesign,cursedcomments,desirepath,doenerverbrechen,dontdeadopeninside,eyebleach,forbiddensnacks,funnyanimals,grssk,hmm,holup,iamatotalpieceofshit,ichbin40undlustig,idiotsincars,illegallysmolcats,instagramreality,instant_regret,kamikazebywords,keming,kidsarefuckingstupid,kitchenconfidential,laughingbuddha,mildlyinfuriating,natureisfuckinglit,nononoyesno,notinteresting,notliketheothergirls,picsofunusualbirds,pizzacrimes,prequelmemes,shitposting,shittyfoodporn,softwaregore,startledcats,stupidfood,techsupportgore,thathappened,trippinthroughtime,unexpected,wewantplates,wasletztepreis,wellthatsucks,whatsthisbug,whatsthisplant,whatswrongwithyourdog,whenthe,whatcouldgowrong,yesyesyesyesno,youseeingthisshit
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
