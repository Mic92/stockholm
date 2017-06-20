{ pkgs, ... }:

let

  msmtprc = pkgs.writeText "msmtprc" ''
    defaults
      logfile ~/.msmtp.log
    account prism
      host prism.r
    account default: prism
  '';

  msmtp = pkgs.writeBashBin "msmtp" ''
    ${pkgs.coreutils}/bin/tee >(${pkgs.notmuch}/bin/notmuch insert +sent) | \
      ${pkgs.msmtp}/bin/msmtp -C ${msmtprc} "$@"
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


    # notmuch
    set nm_default_uri="notmuch://$HOME/Maildir" # path to the maildir
    set nm_record = yes
    set nm_record_tags = "-inbox me archive"
    set virtual_spoolfile=yes                    # enable virtual folders
    set sendmail="${msmtp}/bin/msmtp"                         # enables parsing of outgoing mail
    set from="lassulus@lassul.us"
    set use_from=yes
    set envelope_from=yes

    set index_format="%4C %Z %?GI?%GI& ? %[%d/%b]  %-16.15F %?M?(%3M)&     ? %s %> %?g?%g?"

    virtual-mailboxes \
        "INBOX"     "notmuch://?query=tag:inbox and NOT tag:killed"\
        "Unread"    "notmuch://?query=tag:unread"\
        "TODO"      "notmuch://?query=tag:TODO"\
        "Starred"   "notmuch://?query=tag:*"\
        "Archive"   "notmuch://?query=tag:archive"\
        "Sent"      "notmuch://?query=tag:sent"\
        "Junk"      "notmuch://?query=tag:junk"

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
    macro index t "<modify-labels>+TODO\n"        # tag as Archived


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
      pkgs.neomutt
      (pkgs.writeDashBin "mutt" ''
        exec ${pkgs.neomutt}/bin/mutt -F ${muttrc} $@
      '')
    ];
  };

in {
  environment.systemPackages = [
    msmtp
    mutt
    pkgs.much
    pkgs.notmuch
  ];
}
