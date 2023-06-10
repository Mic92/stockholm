{ config, lib, ... }: let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = true;
    owner = config.krebs.users.jeschli;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (slib.krebs.genipv6 "retiolum" "jeschli" { inherit hostName; }).address;
  });

in {
  hosts = mapAttrs hostDefaults {
    brauerei = {
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.27.29";
          aliases = [
            "brauerei.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAvC4AjkAoH01sKDXE3xVM2YUpPQ9iewIPQCCCSWYZQh2BWOfl+FFs
            pW3ix5FjAzTxzkIf5NxW0usff8UTkFHB+sGZLZ9DPqvb8AM4GJsvXR06LORHtBlo
            Vt/g1sndD3i3NXn5IJ2G4mZDImQjI3vuTkPyFQsR5LRAaPQgIORHBtN/X1UEVMRq
            gThUeMb1kZ/y4AmUx0pepQYmAcYf0cN/7r9n68dWJCZ7DWX3q49bIz4TPG519IQp
            KzoCtdXImKl6cFDepa2pRmIW4SPaDXztHDmXoJA1NBfdDOMOW67FUjzhcwZS9usM
            q9x/1Tph63PJy4Vc0jsJnY29WrInx/nVAb22QuTOXQ9SfBNoOATYoFoVmY+yw1FX
            67y3bRbq8lQk1y3F2vZVYxQ52WiYLmtNtuzUMZHErL7VgFIEfQKoO2Oa/WZXdgSJ
            Asmn67NSicc5QNI4rBUthju1JDuM/3ja0yCXh7trDCmPxKd94KzxMlq9VA6S2f/Q
            uke3VnXEDqOWOZdcon5DnRTT1y4xjk1XHuO/9tVDcrL7x1unkdGL9BNMU6opJiLm
            batAtKQ/7EJrlgIxYpEQyCNAjj0dEn0BgNZNqQSKkeGe6giVMuHtnXeTYMEraDas
            DWxHmGOvYWrs3tZdELkB/h/y7DdijOabS4AlLOljKHiacw8e0D7p9qeIU2EwRaXD
            ebPYaAIIWn1FU1aCYpvF4YJYbdNJZg6aKpoWNz86ZjO9t3GBkf612xB7fRO9mbTg
            Ww2Hl6lir0rnlo7P9M1xhQqmZ0phaUjkqYRCaTOW1kC5ACpJJ/Jrq0oyplHVBY8Y
            IvzPDA4nu/YOpyhQjlQwcVt62NgW0CZdwp3ZnMMoy7akgEo71bjoHbRxAeWy5oRB
            5CgGvQAB+qdf97XjZ5RggWQ2rglkCn49X4fXN6r4zuaIji1VVFTEZGRNsi0vt1YC
            Eedz68auu1ZDO1qwNcX00n94E09B05DQBjE/6SAX6wBCY/BwUtzdQ9JnyfHNSl8i
            dmHBPLssB9Dku4U0mo+LLer+bf6fiR7r5gp/KRuY/tMGFahprZRfWFtyO2Pg1cYI
            HCdmDmSlbFq3EJmBl0egbU8Ym1m6t4EvPcoTxwy3ljZWybHlhm4wvhGcA/2bDRZA
            jcXSL3G7buBOf8WJNYnMXCtPEyIYUdRyNvz3EUfvmbzZDhHd/bc0pJRrrtI7HqoF
            +g67gCrtXx6i9PD0LSDJ1jExMZcmU1+DPg0dzDEmLHvW+HW538/HXGJ8FsunWBwD
            /8wsQfoqAwlBSucLHDDrYVvfSp0+TLzg/HDMhNkcN7d5hm3syrI+IN4gEEjYeZIO
            g7fjR1X7g5FGCDQnRA/dzNsZVnk6UFpCRwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    enklave = {
      ci = false;
      nets = rec {
        internet = {
          ip4.addr = "88.198.164.182";
          aliases = [
            "enklave.i"
          ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.27.30";
          aliases = [
            "enklave.r"
            "cgit.enklave.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIID8gKCA+kAt8zRg/g0jRmqXn6rVul/tdjWtLPcu0aTjNJ5OYZh50i7WqWllGVz
            +FfJicuq/Xd1l5qrgUN7MD+Wrfeov+G9lzSgacfPhXMujutXxX3JwW/9f7UN+yoN
            Sw29Zj+NWb45HyI5WVwMQ332KbKjNcWdTRe+O39oE6bZWg54oEeZOad2UJ7/83sB
            yNEV/B7bJ0+X9HR8XCKrHI/RkjixNauMDlquGzoVyqLKIWwUnBl9CwtNBCYHbvYD
            G1rWeCewd9Z6KsqcKSePfa4mn5eOluWcXmbrD/sx8oII40oNUs3kI7a2HExB2Yle
            P9Q5MQrXRZfI3bdrh1aHieBodZLtosHPNuJIpo8ZaCX88WLhGR3nhJa1vvM1vNwd
            TSSAdobdZUcuIQJKnVxwP4rXQAKPkN2+ddy+tXCGvfFAsdGKDbgPy4FgT+Ed28vg
            3W0fef/3sDNGPY1VAa58/pLz9Un3kNJKUjt00tWamo8daU/3mxZs83nIqDHLq86l
            1+wCl37l+KHe7pUVZ3smoezPRCMoUThmc7VzupbQG+piiSSyiYQi0CuBusa44t76
            1lMr3pOdRBBAoetZ745ZZVx8s+eYk+C1BmQbLJAfzQ9sbH3LAwXpuAH70mtrFqWl
            C3LF89/5mZRbFxALZv9cVx3LqIZDjwpKlwPWorZwo14L+eAagdPCcnVNo6ZcVow2
            mAdNnf7C33fvRsU+rUEIZVPsBHZfAv+f0jqQ65TMvl32VZ0FlxxahSZSj64n8iwr
            Z+DOxKA9OcAaTrHQReYLpWUfNceVDLfOmQLeih8hNgClgqPgYJP/OtN+ox3NP6ZX
            +Gkx9HO7a+agtyJxjh3NYbT/NkRW8HcjW8KgRN7jlE9sQi5/FoxKQOUdHmLTvjdk
            YJXqdPWMYHj2xt4A8x2nzl/si6lwDsod+zdY5RGSdYhoybEOs4wZZIuArmm8GP+C
            IbtgutknAuqvm2FOxyWCbLFTimgqC5BgrNUsXFJJLsHQ3bWFJtVpJlSa5Y0iypCP
            Yr/cefbDrGfs3eCy7FlYDIkCcH06FPm1LTs6USisrtKFObRQN+zPSPln9FysNmpH
            h0YUhrWdTO+wN78K5gc4ALPNUlyqmH61h8jS2qSdrRZLcZWIi4K4banG6EJcWRvV
            kaVxghY1i/Z9x43bZRpBPvpM462IDx08vYX9AcFmF7JfjAXPwJO/EqZVsY1YPDzO
            vdXWrtTORO8R8Pjq3X952yNqgHBcJQh7Q9TBcj+XBtkidOSnTt3Sp/RumsucUW19
            0wMempDPiCOAadLmR4cW5XL1ednXurkd+5gHCmB1Sl7FueP5dgLB/mhXjmITE3zH
            aQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    bolide = {
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.27.31";
          aliases = [
            "bolide.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAn9mkcX4WvyClMxiHgpvM7nNFbGuUVIxM71luzhfroTxMxcWBcik+
            m7ensF256uQeUw4+y/d3wVj06ARnJubdGa1zrM54ghLp6dDTULnUhPfgIbCeP+Zz
            A9hlZsD+yx9ZkPmSMhaakz5dKVlx9KFy4IrS42uGCquXIZ979loZ0372MxYxmPEY
            sIiuZXQcLOiJa5ajJMHS2UEkr4rCvpa/nOJ3AcEl0QbCjX1WALjPZEw9Ogrwergy
            tv0d8dkXHB80DZ4sEjA7+BxB/sVrI4YpT9diTqEHNlkhjYjk6x9o0aBAIfJoyFYK
            Yo2eb8SYN4qwoNbtXr5JLvT0i6thT2R+jXm9h11NIGS91x4cJc2P7eIZI2J33gsV
            VOfcZ4BfJSQSbd1G12EpgzM6UlorNGMqG2DfH3XPbQ0Ez19tNlEUqlbT3dnvVA5y
            kpwWDEJrZB3SDvwtHQf9/4j/jXFlMNu4GE1kcwm+a4LvGFne37atY3irv3xZ9ih4
            ygRbKfFFmxTqK3Cl2Vwxf/dsUm/P+hVF5nzROu7v7wkyU90nYp/AnpXywc4rEAVj
            M8/7H6OlY59yqElJkSdsMum7HtytpsvvenQWW85Ycz+/Ti3nmYuaILgme2Q6JfA3
            c6aNRv8A31m4GIuacHNGJOi2WfJab076bYw82HnX4bVoft9i2SxjZrXr75euz/4k
            jGC4A4YQnrUi4wTBiSc3jm69wb3NXHYoKHjDIwDUUI91vUOKaTO/09RlAYr5EWT3
            nekSaOu9nCrpTR6dq+yc4MktFjHBKPs2ReFKfDHkYzKiNSn5Ei9g3QhpCMF1LE19
            RDvBbFawXtH3M8JHofalwsek44eso0tWA3RzFmiH2WzSLD2UV8a2la9vIfJbMvw3
            LqunwCgC0QWtSTjh+X6H0MBKcGcie5bisKfkXQExx0cnNERDoXpe1E8g5EmJBt2B
            8anRC6thNhpAuMTB8hZy+RP9AsRBxUE9lr9oKDH3JrHRBk1S4LktuNL3Zf8UqnRO
            s7hGPzr8nw5yXV61xXFmXwA0snBsNfo83XpuiWJgz2qrz6/EWLZBgnLNyq22udxl
            O9EGExulbija5rmrSp1MGi7K2K2qAx06KP3C1TerpSY0BW6ZhX/4MK/WIRihDBG4
            JLg/gxX/4FES2qGmjCkW74TQDHq6B8xFuJQ3y5nD1cZBN4+VDNb1+mQbR06Zn0Sc
            pDASYWimJzqxR+s/cr0a8PwoE/RdjwOYlXIOW4tDcK12Am6XRRf87cycG9ueNzhy
            RwrvUJ4JMyUA5sbo2sbumAMqiytNZ7ReRVK6S5IiMpysr4/lHb2OQSPhQArDCprg
            KXNgncRZsrL9VgVueVmOp45qkAkapeQ8wQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    reagenzglas = {
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.27.32";
          aliases = [
            "reagenzglas.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAwYLQr5m/x7Q6w8sL8QwI
            GSEScP4V1Eun77mpV5ygk8FBV7S7Cj64QaEntM4NPNMS8DOs3qDkXQbC5yQQhVdG
            rL191UK7B1VxprsyVDY5wj2bR96vOX9KadYSpk2lEaB2yyA8Xt2t4VrhcDOIk3k+
            COMIbeqFd4rs5B2kV8p1KIFScng0x7uDhEvpn8zTakbtXWzcqirzBzqLTt9GqHE9
            wXAca7iYCabhp6tzrOF7ifkRXgFy9+RPjUb8cqOyYL0k4zRSqNRDUQaySgUHRUwo
            EzE/piLnBQHX/7tc9PdNPHizL62HeLOCnsKB+MoaJqsey6KPhxVDwYw3dJ/C3pCS
            wFMUlt6D/5LxPL2yXJRRGb+I/hLnKGzbfB7Hz4Mh2PW2NMtdK0NMouDdH1VRnx68
            QdL0MetHECz+TjpZIrn0Y2OCizDDGiKDndafQi4VPnWGulYUHtpIIMHkAS6xCHHn
            5Rfe1LRxNXVSfqcQEYbjf+PNmwUw8etzBwYzB6zFFnQhw+6kWBPqnB38NkQ0Fzhc
            h7isl2iq9aotObk9p53gj1i8eaSCeq6C5sFM9Bs3d00HfCLNTCNMqYZynmapo+3Q
            0P6oX3YWzM2oUiknWKKOVyDUwCJolwlAeNOvlwCDzsiAAAB7INYBnJCIIPPcoE/q
            iddgcSx6Poq15h8H5tr439kCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
  };
  users = {
    jeschli = {
      mail = "jeschli@gmail.com";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDMPuFzd6p3zZETIjoV5mRxCTQgeZk9s/P374mEDbj58wDTT0uGWu2JRf7cL1QRTvd5238tYl0eSHXH65+oaFB/mIvmiRnuw6qQODOMHlSbJN5/J2hEw/3v5gveiP1xNLfKlFhj6mmMRF7Etvzns/kLGLCSjj1UTlfo4iHmtinPmU+iQ8J4foS4cZj4oZesF8gndkc2EFMfL6en7EuU8GK6U9GtwKNL9N4UoUZXu8Nf00pkn/jrpmsDdI4zdVVAxWeu/Lo4li43EVixLcfwQiwzf6S9FvYIv30xPdy92GJSJwxm/QkYuc48VZWUoE+qThf3IEPETtX+MRZrM8RTtY01";
    };
    jeschli-bln = {
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDhQdDQFMxXOjbC+Avx3mlcFHqQpFUk/q9sO6ATA65jCV3YzN11vhZDDv54hABVS2h8TPXs7Lu3PCvK9qouASd2h4Ie9cExUmn50G/iwgFIODsCugVYBzVt1iwaAdwz1Hb9DKYXbVXanzVJjimmrrlQNvsyZg85lcnfyedpPX5ad+4FdSP68LHqEHC18LTitldR6V4P1omaKHlOtVpDgR/72tDgbtNZDBn3EU+TPk9OLTzjc6PinPw4iIvjEfiu14APwXpFDIqT7P7SjOEFpa0v/1z7dhxIy/Z9XbqyEdUfhv3PjZR5K2C+VzR7g6jVEVR2xFId51MpLv/Un4/lalbphBEw3I90Rr8tatOJiFhyrXbaKTcLqp1sIu05OxdPkm3hzfmLIhoKxhaIlXH7WQ9sAqxL1NAQ7O+J6yT4DMnwKzvpkkJjBaGtV84Pp1cccfNRH8XXID3FkWkrUpdgXWBpyLnRq4ilUJTajkU0GSdXkq8kLL3mWg9LPRTg3dmDj61ZB/qhjM61ppwHJvDRN9WI5HruXIU6nOQjh5yE2C/JZfLcsZD4Y1UDBy5/JSZrCVT2sQjFopkkYEkRCbX7oITHOH4iyRdxZkKWLUPboFrcmBpXO+owCEhO4JZrtfFWMC6qM++nrmiZWOrdIOIvdYHWluhKR2shlkisEKQP5pUqkw== markus.hihn@dcso.de";
    };
   jeschli-brauerei = {
     pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEZgHR1ZPDBMUjGWar/QmI2GiUkZM8pAXRyBDh8j3hGlxlS+0lsBV6bTAI5F13iyzTC4pCuEuDO2OlFB0scwjcOATci8phd8jTjOIDodqDaeQZXbshyuUBfyiAV6q0Sc+cUDV3D6GhzigH3t8EiQmvXmUGm916yFotT12o0dm83SCOh1nAf9ZveC1Hz/eEUTvgWvIb58OdUR5F/S5OVBnIIJZ8tcp0BP9lyjjJCcANWkYJlwaVcNNb0UarCRhvRtptFj+e/EPqQxSCaS2QcxW4zBsQ6C81TFf7WrdH+pwtFg0owlWsxv547sRLLiPf2h2YuQgSoAaW24N0SHhUqvOXd+JyaYw7MAF8Qh3jHm2iJQRgXNuIN0msFi1alwAevilL2mnfAt2biQ9sS9g+CVvQCwX3mg09E4Y3UmFLzvsJafD9meKVrjnDCcXySeAfts59eFmwKtMQ0qrEWaclzUiA6Ay3uD1zma8x1XELGTf8nxnXCGl8s2i2APn7y1Tcwep69DlENWSaReF5zBLIkCtIUDd+8xBFTF3yu5CpyRrRMKGa0QX/MtsQl4SGJWadOTwpM8joIbrIVfKkTNB2McxAjvo0iaRoBDm409gi2Ycy+NSoUV/KAIUG7OysAQZ62hr+E/Kw1ocJCIVI+9vzKx/EnEIHkCSwhYKl5393W7CShVJjJUcKcZddqX2smSShXq8rXPzhIHk1dAVn5Ff/vGZT9z9R0QN3z6Oa9QN5t5TjTdUDToqHTudqOpDxPl2c2yXK9wV+aoHFoML9AmbzTT1U1mKU7GXSoFACiKNzhDzkovyJGpWRyvisX5t75IfuVqvGGI8n3u8OhPMdyyOHRylVaciDzBMZ00xnIHB+dJG9IeYaMm9bW1Li4Jo0CWnogo2+olfHPMLijBuu+bsa5Kp6kFkccJYR/xqcSq0lVXkpGm692JI4dnMGjchipXEGh1gXof9jXHemMMBwjpLFGty+D0r5KdA33m+mIqc9hi0ShquA9nA7E1IxDlgE0gQg+P5ZOeeIN7q54AQmT8iCCCRyne2Kw57XxaGgZoLfj7VjjaeRlzBUglmtyq8B7/c0J3y41vt9Hxhj4sKD+vufZu+M9E6E936KsJlIi+3U0PtopM/b8L4jcH1JYpPljapsys8wkJZ1ymHf6Kj/0FHyi1V+GvquiVrlFN+aHECIzNlCiSMO4MqfPUO1A+s9zkG2ZgPNNv+LoZqnokjbmKM4kdxexMxaL/Eo9Nd/bzdYiFYXlllEL7Uox+yV0N3loQ2juh4zn+ctCnwHi+V9X4l4rB8amW96WrXiJ/WqEK2UO8St8dcQWhCsUUm2OawSrbYYZw5HhJwz/Rhz2UsdSc56s5OUiQLJqpILYvCnqSLlF4iZdRSdDQNpKn+le3CeGUl5UUuvK2BpKGrbPKx0i/2ZSEMxNA5GnDMx/NyiNyDBcoPu/XOlNi8VWsEbCtoTQRamvqHjOmNcPrxCxds+TaF8c0wMR720yj5sWq8= jeschli@nixos";
   };
   jeschli-bolide = {
     pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDGnLjnFw3OYJJy/ID9RCWHTtnVcKRfROIj1tJdJZoOWzGMY+kgyCN/WNBg7JJtCW22yU5O3ftSdN851URCBZ6FgYmcvURBxUOKPlyX5EoxUrrnbmtxJM9+OIT3Dt2RWfrqX6aEQ57nwe/qIFKo9UaoedB/FOzsw1f3U5zBxVuWVRQrsnPxkbPWHmPAGB4CvL897tb83uecDexmGZpLe/0dN4768i2nYaSwrNL+HtqZCvkEqEmnfHlmqqXhiuq83q8su/WSXAtDbUVucG3frgOir14YCbrWKf59+MugxhYOEYBqp+KME5+niFGoulg+NBW/HzH6U+DiH4RFBJhCu1Gr jeschli@bolide";
   };
   jeschli-reagenzglas = {
     pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFXgtbgeivxlMKkoEJ4ANhtR+LRMSPrsmL4U5grFUME jeschli@nixos";
   };
  };
}
