{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  cfg = config.tv.ejabberd;

  out = {
    options.tv.ejabberd = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.ejabberd";

    certFile = mkOption {
      type = types.str;
      default = toString <secrets/ejabberd.pem>;
    };

    hosts = mkOption {
      type = with types; listOf str;
    };
  };

  imp = {
    environment.systemPackages = [ my-ejabberdctl ];

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        PermissionsStartOnly = "true";
        SyslogIdentifier = "ejabberd";
        User = user.name;
        ExecStartPre = pkgs.writeScript "ejabberd-start" ''
          #! /bin/sh
          install -o ${user.name} -m 0400 ${cfg.certFile} /etc/ejabberd/ejabberd.pem
        '';
        ExecStart = pkgs.writeScript "ejabberd-service" ''
          #! /bin/sh
          ${my-ejabberdctl}/bin/ejabberdctl start
        '';
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
      home = "/var/ejabberd";
      createHome = true;
    };
  };

  user = {
    name = "ejabberd";
    uid = 3499746127; # genid ejabberd
  };

  my-ejabberdctl = pkgs.writeScriptBin "ejabberdctl" ''
    #! /bin/sh
    set -euf
    exec env \
        SPOOLDIR=/var/ejabberd \
        EJABBERD_CONFIG_PATH=${config-file} \
      ${pkgs.ejabberd}/bin/ejabberdctl \
        --logs /var/ejabberd \
        "$@"
  '';

  config-file = pkgs.writeText "ejabberd.cfg" ''
    {loglevel, 3}.
    {hosts, ${toErlang cfg.hosts}}.
    {listen,
     [
      {5222, ejabberd_c2s, [
          starttls,
          {certfile, "/etc/ejabberd/ejabberd.pem"},
          {access, c2s},
          {shaper, c2s_shaper},
          {max_stanza_size, 65536}
               ]},
      {5269, ejabberd_s2s_in, [
             {shaper, s2s_shaper},
             {max_stanza_size, 131072}
            ]},
      {5280, ejabberd_http, [
           captcha,
           http_bind,
           http_poll,
           web_admin
          ]}
     ]}.
    {s2s_use_starttls, required}.
    {s2s_certfile, "/etc/ejabberd/ejabberd.pem"}.
    {auth_method, internal}.
    {shaper, normal, {maxrate, 1000}}.
    {shaper, fast, {maxrate, 50000}}.
    {max_fsm_queue, 1000}.
    {acl, local, {user_regexp, ""}}.
    {access, max_user_sessions, [{10, all}]}.
    {access, max_user_offline_messages, [{5000, admin}, {100, all}]}.
    {access, local, [{allow, local}]}.
    {access, c2s, [{deny, blocked},
             {allow, all}]}.
    {access, c2s_shaper, [{none, admin},
              {normal, all}]}.
    {access, s2s_shaper, [{fast, all}]}.
    {access, announce, [{allow, admin}]}.
    {access, configure, [{allow, admin}]}.
    {access, muc_admin, [{allow, admin}]}.
    {access, muc_create, [{allow, local}]}.
    {access, muc, [{allow, all}]}.
    {access, pubsub_createnode, [{allow, local}]}.
    {access, register, [{allow, all}]}.
    {language, "en"}.
    {modules,
     [
      {mod_adhoc,    []},
      {mod_announce, [{access, announce}]},
      {mod_blocking,[]},
      {mod_caps,     []},
      {mod_configure,[]},
      {mod_disco,    []},
      {mod_irc,      []},
      {mod_http_bind, []},
      {mod_last,     []},
      {mod_muc,      [
          {access, muc},
          {access_create, muc_create},
          {access_persistent, muc_create},
          {access_admin, muc_admin}
         ]},
      {mod_offline,  [{access_max_user_messages, max_user_offline_messages}]},
      {mod_ping,     []},
      {mod_privacy,  []},
      {mod_private,  []},
      {mod_pubsub,   [
          {access_createnode, pubsub_createnode},
          {ignore_pep_from_offline, true},
          {last_item_cache, false},
          {plugins, ["flat", "hometree", "pep"]}
         ]},
      {mod_register, [
          {welcome_message, {"Welcome!",
                 "Hi.\nWelcome to this XMPP server."}},
          {ip_access, [{allow, "127.0.0.0/8"},
                 {deny, "0.0.0.0/0"}]},
          {access, register}
         ]},
      {mod_roster,   []},
      {mod_shared_roster,[]},
      {mod_stats,    []},
      {mod_time,     []},
      {mod_vcard,    []},
      {mod_version,  []}
     ]}.
  '';


  # XXX this is a placeholder that happens to work the default strings.
  toErlang = builtins.toJSON;

in
out
