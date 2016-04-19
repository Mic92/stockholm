{ pkgs, ... }:

let

  msmtprc = pkgs.writeText "msmtprc" ''
    defaults
      logfile ~/.msmtp.log
    account prism
      host prism.r
    account default: prism
  '';

  msmtp = pkgs.writeScriptBin "msmtp" ''
    ${pkgs.msmtp}/bin/msmtp -C ${msmtprc} $@
  '';

  muttrc = pkgs.writeText "muttrc" ''
    # notmuch
    set nm_default_uri="notmuch://$HOME/Maildir" # path to the maildir
    set nm_record = yes
    set nm_record_tags = "-inbox me archive"
    set virtual_spoolfile=yes                    # enable virtual folders
    set sendmail="msmtp"                         # enables parsing of outgoing mail
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
    macro index A "<modify-labels>+archive -unread -inbox\n"  # tag as Archived
    macro index + "<modify-labels>+*\n<sync-mailbox>"         # tag as starred
    macro index - "<modify-labels>-*\n<sync-mailbox>"         # tag as unstarred


    #killed
    bind index d noop
    bind pager d noop

    bind pager S noop
    macro index S "<modify-labels-then-hide>-inbox -unread +junk\n" # tag as Junk mail
    macro pager S "<modify-labels-then-hide>-inbox -unread +junk\n" # tag as Junk mail

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

  mutt = pkgs.writeScriptBin "mutt" ''
    ${pkgs.mutt-kz}/bin/mutt -F ${muttrc} $@
  '';

in {
  environment.systemPackages = [
    msmtp
    mutt
    pkgs.much
    pkgs.notmuch
  ];
}
