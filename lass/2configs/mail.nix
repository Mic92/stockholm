with import <stockholm/lib>;
{ pkgs, ... }:

let

  msmtprc = pkgs.writeText "msmtprc" ''
    defaults
      logfile ~/.msmtp.log
    account prism
      host prism.r
    account c-base
      from lassulus@c-base.org
      host c-mail.c-base.org
      port 465
      tls on
      tls_starttls off
      tls_fingerprint 8C:10:A6:AB:1F:82:C4:8F:B1:B4:22:D5:8B:8B:49:9B:59:0B:22:A4
      auth on
      user lassulus
      passwordeval pass show c-base/pass
    account default: prism
  '';

  msmtp = pkgs.writeBashBin "msmtp" ''
    ${pkgs.coreutils}/bin/tee >(${pkgs.notmuch}/bin/notmuch insert +sent) | \
      ${pkgs.msmtp}/bin/msmtp -C ${msmtprc} "$@"
  '';

  mailcap = pkgs.writeText "mailcap" ''
    text/html; ${pkgs.elinks}/bin/elinks -dump ; copiousoutput;
  '';

  mailboxes = {
    c-base = [ "to:c-base.org" ];
    coins = [
      "to:btce@lassul.us"
      "to:coinbase@lassul.us"
      "to:polo@lassul.us"
      "to:bitwala@lassul.us"
      "to:payeer@lassul.us"
      "to:gatehub@lassul.us"
      "to:bitfinex@lassul.us"
      "to:binance@lassul.us"
      "to:bitcoin.de@lassul.us"
      "to:robinhood@lassul.us"
    ];
    dezentrale = [ "to:dezentrale.space" ];
    dhl = [ "to:dhl@lassul.us" ];
    github = [ "to:github@lassul.us" ];
    gmail = [ "to:gmail@lassul.us" "to:lassulus@gmail.com" "lassulus@googlemail.com" ];
    kaosstuff = [ "to:gearbest@lassul.us" "to:banggood@lassul.us" "to:tomtop@lassul.us" ];
    nix-devel = [ "to:nix-devel@googlegroups.com" ];
    patreon = [ "to:patreon@lassul.us" ];
    paypal = [ "to:paypal@lassul.us" ];
    ptl = [ "to:ptl@posttenebraslab.ch" ];
    retiolum = [ "to:lass@mors.r" ];
    security = [ "to:seclists.org" "to:bugtraq" "to:securityfocus@lassul.us" ];
    shack = [ "to:shackspace.de" ];
    steam = [ "to:steam@lassul.us" ];
    tinc = [ "to:tinc@tinc-vpn.org" "to:tinc-devel@tinc-vpn.org" ];
    wireguard = [ "to:wireguard@lists.zx2c4" ];
    zzz = [ "to:pizza@lassul.us" "to:spam@krebsco.de" ];
  };

  tag-new-mails = pkgs.writeDashBin "nm-tag-init" ''
    ${pkgs.notmuch}/bin/notmuch new
    ${concatMapStringsSep "\n" (i: ''${pkgs.notmuch}/bin/notmuch tag -inbox +${i.name} -- tag:inbox ${concatMapStringsSep " or " (f: "${f}") i.value}'') (mapAttrsToList nameValuePair mailboxes)}
  '';

  tag-old-mails = pkgs.writeDashBin "nm-tag-old" ''
    ${concatMapStringsSep "\n" (i: ''${pkgs.notmuch}/bin/notmuch tag -inbox -archive +${i.name} -- ${concatMapStringsSep " or " (f: "${f}") i.value}'') (mapAttrsToList nameValuePair mailboxes)}
  '';

  muttrc = pkgs.writeText "muttrc" ''
    # gpg
    source ${pkgs.neomutt}/share/doc/mutt/samples/gpg.rc
    set pgp_use_gpg_agent = yes
    set pgp_sign_as = 0x976A7E4D
    set crypt_autosign = yes
    set crypt_replyencrypt = yes
    set crypt_verify_sig = yes
    set pgp_verify_command = "gpg --no-verbose --batch --output - --verify %s %f"

    macro index \Cv \
    "<enter-command> set my_crypt_verify_sig=\$crypt_verify_sig<enter> \
    <enter-command> set crypt_verify_sig=yes<enter> \
    <display-message><enter-command> set crypt_verify_sig=\$my_crypt_verify_sig<enter>" \
     'Verify PGP signature and open the message'

    macro pager \Cv \
    "<exit><enter-command> set my_crypt_verify_sig=\$crypt_verify_sig<enter> \
    <enter-command> set crypt_verify_sig=yes<enter> \
    <display-message><enter-command> set crypt_verify_sig=\$my_crypt_verify_sig<enter>" \
     'Verify PGP signature'

    # read html mails
    auto_view text/html
    set mailcap_path = ${mailcap}

    # notmuch
    set nm_default_uri="notmuch://$HOME/Maildir" # path to the maildir
    set nm_record = yes
    set nm_record_tags = "-inbox me archive"
    set virtual_spoolfile=yes                    # enable virtual folders


    set sendmail="${msmtp}/bin/msmtp"            # enables parsing of outgoing mail
    set from="lassulus@lassul.us"
    alternates ^.*@lassul\.us$ ^.*@.*\.r$
    set use_from=yes
    set envelope_from=yes
    set reverse_name

    set sort=threads

    set index_format="${pkgs.writeDash "mutt-index" ''
      # http://www.mutt.org/doc/manual/#formatstrings
      recipent="$(echo $1 | sed 's/[^,]*<\([^>]*\)[^,]*/ \1/g')"
      #     output to mutt
      #           V
      echo "%4C %Z %?GI?%GI& ? %[%y-%m-%d] %-20.20a %?M?(%3M)& ? %s %> $recipent %?g?%g?%"
      # args to mutt-index dash script
      # V
    ''} %r |"

    virtual-mailboxes "INBOX" "notmuch://?query=tag:inbox"
    virtual-mailboxes "Unread" "notmuch://?query=tag:unread"
    ${concatMapStringsSep "\n" (i: ''${"  "}virtual-mailboxes "${i.name}" "notmuch://?query=tag:${i.name}"'') (mapAttrsToList nameValuePair mailboxes)}
    virtual-mailboxes "TODO" "notmuch://?query=tag:TODO"
    virtual-mailboxes "Starred" "notmuch://?query=tag:*"
    virtual-mailboxes "Archive" "notmuch://?query=tag:archive"
    virtual-mailboxes "Sent" "notmuch://?query=tag:sent"
    virtual-mailboxes "Junk" "notmuch://?query=tag:junk"
    virtual-mailboxes "All" "notmuch://?query=*"

    tag-transforms "junk"     "k" \
                   "unread"   "u" \
                   "replied"  "↻" \
                   "TODO"     "T" \

    # notmuch bindings
    macro index \\\\ "<vfolder-from-query>"                   # looks up a hand made query
    macro index + "<modify-labels>+*\n<sync-mailbox>"         # tag as starred
    macro index - "<modify-labels>-*\n<sync-mailbox>"         # tag as unstarred


    #killed
    bind index d noop
    bind pager d noop

    bind index S noop
    bind index s noop
    bind pager S noop
    bind pager s noop
    macro index S "<modify-labels-then-hide>-inbox -unread +junk\n" # tag as Junk mail
    macro index s "<modify-labels>-junk\n" # tag as Junk mail
    macro pager S "<modify-labels-then-hide>-inbox -unread +junk\n" # tag as Junk mail
    macro pager s "<modify-labels>-junk\n" # tag as Junk mail


    bind index A noop
    bind index a noop
    bind pager A noop
    bind pager a noop
    macro index A "<modify-labels>+archive -unread -inbox\n"  # tag as Archived
    macro index a "<modify-labels>-archive\n"  # tag as Archived
    macro pager A "<modify-labels>+archive -unread -inbox\n"  # tag as Archived
    macro pager a "<modify-labels>-archive\n"  # tag as Archived


    bind index t noop
    bind pager t noop
    macro index t "<modify-labels>"        # tag as Archived

    # top index bar in email view
    set pager_index_lines=7

    # sidebar
    set sidebar_width   = 20
    set sidebar_visible = yes               # set to "no" to disable sidebar view at startup
    color sidebar_new yellow default
    # sidebar bindings
    bind index <left> sidebar-prev          # got to previous folder in sidebar
    bind index <right> sidebar-next         # got to next folder in sidebar
    bind index <space> sidebar-open         # open selected folder from sidebar
    # sidebar toggle
    macro index ,@) "<enter-command> set sidebar_visible=no; macro index ~ ,@( 'Toggle sidebar'<Enter>"
    macro index ,@( "<enter-command> set sidebar_visible=yes; macro index ~ ,@) 'Toggle sidebar'<Enter>"
    macro index ~ ,@( 'Toggle sidebar'      # toggle the sidebar
  '';

  mutt = pkgs.symlinkJoin {
    name = "mutt";
    paths = [
      (pkgs.writeDashBin "mutt" ''
        exec ${pkgs.neomutt}/bin/mutt -F ${muttrc} $@
      '')
      pkgs.neomutt
    ];
  };

in {
  environment.systemPackages = [
    msmtp
    mutt
    pkgs.much
    pkgs.notmuch
    tag-new-mails
    tag-old-mails
  ];
}
