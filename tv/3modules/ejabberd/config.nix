with import <stockholm/lib>;
{ config, ... }: let

  # See https://github.com/processone/ejabberd/blob/master/ejabberd.yml.example

  ciphers = concatStringsSep ":" [
    "ECDHE-ECDSA-AES256-GCM-SHA384"
    "ECDHE-RSA-AES256-GCM-SHA384"
    "ECDHE-ECDSA-CHACHA20-POLY1305"
    "ECDHE-RSA-CHACHA20-POLY1305"
    "ECDHE-ECDSA-AES128-GCM-SHA256"
    "ECDHE-RSA-AES128-GCM-SHA256"
    "ECDHE-ECDSA-AES256-SHA384"
    "ECDHE-RSA-AES256-SHA384"
    "ECDHE-ECDSA-AES128-SHA256"
    "ECDHE-RSA-AES128-SHA256"
  ];

  protocol_options = [
    "no_sslv2"
    "no_sslv3"
    "no_tlsv1"
    "no_tlsv1_10"
  ];

in /* yaml */ ''

  access_rules:
    announce:
      - allow: admin
    local:
      - allow: local
    configure:
      - allow: admin
    register:
      - allow
    s2s:
      - allow
    trusted_network:
      - allow: loopback

  acl:
    local:
      user_regexp: ""
    loopback:
      ip:
        - "127.0.0.0/8"
        - "::1/128"
        - "::FFFF:127.0.0.1/128"

  certfiles:
    - /tmp/credentials/certfile

  hosts: ${toJSON config.hosts}

  language: "en"

  listen:
    -
      port: 5222
      ip: "::"
      module: ejabberd_c2s
      shaper: c2s_shaper
      ciphers: ${toJSON ciphers}
      dhfile: ${config.stateDir}/dhfile
      protocol_options: ${toJSON protocol_options}
      starttls: true
      starttls_required: true
      tls: false
      tls_compression: false
      max_stanza_size: 65536
    -
      port: 5269
      ip: "::"
      module: ejabberd_s2s_in
      shaper: s2s_shaper
      max_stanza_size: 131072

  loglevel: 4

  modules:
    mod_adhoc: {}
    mod_admin_extra: {}
    mod_announce:
      access: announce
    mod_caps: {}
    mod_carboncopy: {}
    mod_client_state: {}
    mod_configure: {}
    mod_disco: {}
    mod_echo: {}
    mod_bosh: {}
    mod_last: {}
    mod_offline:
      access_max_user_messages: max_user_offline_messages
    mod_ping: {}
    mod_privacy: {}
    mod_private: {}
    mod_register:
      access_from: deny
      access: register
      ip_access: trusted_network
      registration_watchers: ${toJSON config.registration_watchers}
    mod_roster: {}
    mod_shared_roster: {}
    mod_stats: {}
    mod_time: {}
    mod_vcard:
      search: false
    mod_version: {}
    mod_http_api: {}

  s2s_access: s2s
  s2s_ciphers: ${toJSON ciphers}
  s2s_dhfile: ${config.stateDir}/dhfile
  s2s_protocol_options: ${toJSON protocol_options}
  s2s_tls_compression: false
  s2s_use_starttls: required

  shaper_rules:
    max_user_offline_messages:
      - 5000: admin
      - 100
    max_user_sessions: 10
    c2s_shaper:
      - none: admin
      - normal
    s2s_shaper: fast
''
