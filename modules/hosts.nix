{ config, pkgs, ... }:

{
  networking.extraHosts =
    ''
      192.168.1.1 wrt.gg23 wrt
      192.168.1.11 mors.gg23
      192.168.1.12 uriel.gg23
      192.168.1.23 raspi.gg23 raspi
      192.168.1.37 wu.gg23
      192.168.1.110 nomic.gg23
      192.168.1.124 schnabeldrucker.gg23 schnabeldrucker

      127.0.0.1  dev.zalora.sg www.dev.zalora.sg bob.dev.zalora.sg static.dev.zalora.sg
      127.0.0.1  dev.zalora.com.my www.dev.zalora.com.my bob.dev.zalora.com.my static.dev.zalora.com.my
      127.0.0.1  dev.zalora.com.ph www.dev.zalora.com.ph bob.dev.zalora.com.ph static.dev.zalora.com.ph
      127.0.0.1  dev.zalora.vn www.dev.zalora.vn bob.dev.zalora.vn static.dev.zalora.vn
      127.0.0.1  dev.zalora.co.id www.dev.zalora.co.id bob.dev.zalora.co.id static.dev.zalora.co.id
      127.0.0.1  dev.zalora.co.th www.dev.zalora.co.th bob.dev.zalora.co.th static.dev.zalora.co.th
      127.0.0.1  dev.zalora.com.hk www.dev.zalora.com.hk bob.dev.zalora.com.hk static.dev.zalora.com.hk

      54.93.104.95 eu-dev.hk.zalora.net www.eu-dev.hk.zalora.net bob.eu-dev.hk.zalora.net static.eu-dev.hk.zalora.net
      54.93.104.95 eu-dev.sg.zalora.net www.eu-dev.sg.zalora.net bob.eu-dev.sg.zalora.net static.eu-dev.sg.zalora.net
    '';
}
