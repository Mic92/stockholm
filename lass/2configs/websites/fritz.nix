{ config, pkgs, lib, ... }:

let
  inherit (import ../../4lib { inherit lib pkgs; })
    manageCert
    activateACME
    ssl
    servePage
    serveOwncloud;

in {
  imports = [
    ( manageCert "biostase.de" )
    ( servePage "biostase.de" )

    ( manageCert "gs-maubach.de" )
    ( servePage "gs-maubach.de" )

    ( manageCert "spielwaren-kern.de" )
    ( servePage "spielwaren-kern.de" )

    ( manageCert "societyofsimtech.de" )
    ( servePage "societyofsimtech.de" )

    ( manageCert "ttf-kleinaspach.de" )
    ( servePage "ttf-kleinaspach.de" )

    ( manageCert "edsn.de" )
    ( servePage "edsn.de" )

    ( manageCert "eab.berkeley.edu" )
    ( servePage "eab.berkeley.edu" )

    ( manageCert "habsys.de" )
    ( servePage "habsys.de" )
  ];

  #lass.owncloud = {
  #  "o.ubikmedia.de" = {
  #    instanceid = "oc8n8ddbftgh";
  #  };
  #};

  #services.mysql = {
  #  enable = true;
  #  package = pkgs.mariadb;
  #  rootPassword = toString (<secrets/mysql_rootPassword>);
  #};
}
