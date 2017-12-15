{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.jeschli;
    ci = true;
  }) {

    bln = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.27.28";
          ip6.addr = "42::28";
          aliases = [
            "bln.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAwoN2f6iyQ1Wnk4rZVqhovny8VpwWvC9buE+NoedRaxmWmA5QIP02
            BLwTWFKnbiKOQiYN+a4m/JKs0fFOjYCa2EKhqWWKwdEIN4wJTq8zrjzIaa2rdz+8
            tamE+8rSYDE+RbJ6Gs3SUDfwcxJT6FXCi3JYoirdhAssLSwTf9d5IsfXvkKMabky
            FpY9Im51utmIR8UmYL4Ti7dEaOxif+5Hgl1LuitC8e2IIZJhXJprK9tJk9J0LRWt
            PUM31IG1+A2hNBzs5hferLmmwFvYF1sJ22NtFepxVyOLaLcLEFKWHyU+14qEMSgL
            acsu0lgVZ4A1TY6vVBmawfVCzUzRfalNIty1x+qDA4MB1RQ4W7ivWCjd/+wirSyc
            BLxCvriXRdUwPIRoHy0kNMmS83HGm2iv2IrHUrcH8lyJvMys216J2lCF2arRVnBn
            lArObfR3mXgd/YoANmZ4cinLAjLCjCjXfOe39+pvTFph6WnDt4gOO+tQlnCk19Fa
            NoiK1THcuZiFVE+4CAXVmstNqYKSMgw+Upw7/t6iUzur98iwKpcicomhJjGVVtbg
            2iDf4lYVrUyb7iPns2T4EzAuHk7iESktEASU5creSbWYRu/4uyhuNlUoiCpVOEKg
            H9jkrLlCpQGv/GmgdH9oj35Dsv5TINauCT2jjWV65wcKAlvyafy5UtLyF4HBRHaM
            2xyxC9gxr8bmeOFyOnHVJQvpkeLxyaRp/VppjCTzr82TQvpZd5a+tISIbDGfqX1o
            cEyPsowb3KHNtW9DqRBp+80fPGnQHsNjVXbJb37wjpnR/ePg/XyENbZF/OQEsjqt
            bki8hZQXKJAFyx1bq/2A1q4ocx7JlJKynL4szG1unHbSPKNH2OOVvoezuP7e+lXU
            gnzrSbe9lPIOp4Vu1HjWOi6tNWZFoZrSHVIK+VGxm+wm/HoS+Enj4Yq+vRvU3luv
            UllR5KHHK2970RbFEUE0zaVMZjQn5KgJjFXfqfrCztp0wZ5CQo+tRFPq35llaIQ2
            0WyT2IZlxt1Xr2IpOM0DpO4SJnivZT/wdZN7upzsUPf4a9suztpA3KcKAKqH0OM5
            fv2/LXspc73vACAOZ9qDJnwp8bFrMOaQdAL1oPpOLB3yYTDA3E20IAQ6OKoSy1Nl
            B4coqo1gBCcMrWwVFYAuc5J4itXJ0SSj67+WUnuDzPm88LI3g+AO0r1m6k6YdA58
            SeNxYPMLYNLRg86rsjKjXu+QyvBsd04O/QvIxpTFCtdjbUXNS1H4++/inYZSwWPp
            U0lN9erLJbwr4WqU/Mn6J+jKijXwmCSiF5if5baszMsOL/0u9yFt6OcaLyehE3sJ
            eAo00n9phSna0lxtbtRnh/Gd4D7rFcX33wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    reagenzglas = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.27.27";
          ip6.addr = "42::27";
          aliases = [
            "reagenzglas.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEA4Tbq6aiMhPz55Of/WDEmESGmScRJedQSJoyRuDEDabEktdbP/m7P
            bwpLp0lGYphx42+lutFcYOGoH/Lglfj39yhDcrpkYfTnzcGOWutXGuZ+iR5vmGj0
            utJRf/4+a4sB5NboBVZ9Ke/RTjDNSov00C2hFCYTXz89Gd2ap1nDPQpnejOS+9aO
            2W6P/WIKhRH7TfW6M7mUCrjVxWXZgdfSCQYxAXU/+1uAGmJ9qlGlQUIGUGv9Znv5
            hurqwAHzSgEkKc2iUumosz6a8W9Oo3TAEC+jMEO2l/+GJ/8VysG1wtLWDX03GU3u
            mBAtrJppEw4QNPTeFg6XSFIwV8Z0fWZ4lGsPJLbAkLUMxtKVWKbdrdpnmiQpLfBW
            8BRbT1pjwEdw0hefA6NwCO3/Y5piEaUEz/gYz9xHFMDXUj9stHtaF0HaqonWyb06
            aX3EEqRBxVsj6/Sgd33b77xqY4WBoOlbhfWj+EAD1Ova26lHELpAg0Z4AncpyOzw
            pJcX81U8GgQp899YAc3EAldFfiu094CvM2NKd110K90VlTpos+sqFfNE87vpprMu
            3d1NsYzf+FUM/aXASlqTNL+i8qBDAlODkLdj4+VZ2BjkSH+p2BLZouizSzu4X3I/
            lfy554Dbb/98zlwmX9JrWzBRs2GxxFdIDZ1jK+Ci5qM7oTfujBwiE4jZA6wlK8u5
            +IenSBdaJb0J8nS0Bziz/BLkuBCrl/YFelpZlY0pw6WYlraKbf/nsOpumOYh6zdz
            9jiIPElGvso9FhwigX7xWCiYMK3ryAqm8CL0cTscQW3Yy2JKm1tNIQtAacwnNVli
            PqdnPJSo942I+Fl6ZPjZ19ivJIqC+2TjGEY2Et8DkiL6YZfy4bM1zhoWMlXBIil0
            ynnKR/h/CC67cq94JCbtRWKiYXIYtfHPQkS7S1Lk6aSYbIch/wROyh7XJ7EGE7nn
            GAVMqI/P/qbW3rwEJGXzI4eJAHa2hwpP2Slimf6uUD/6L2bAnduhYoTsnNSjJmNE
            hCC+MHohzk7+isZl0jwIEcMpsohMAwoa5BEhbuYJWeUesT/4PeddLIGYubTZAXp2
            ZdYRepSNUEhSZV0H99MhlqeooDJxnWpsiba5Gb0s6p4gTReGy0jMtWnxI2P5RUFX
            vEGt77v4MGrWYTzAL/ZRmESsOj7TXqpSK5YcMC2nr8PcV66LuMjOkRrGoVOV3fBe
            G/9pNVb68SRwfPoGa5nGe6C7GPcgko9rgGLLcU1r/4L2bqFhdIQdSfaUX2Hscm44
            5GdN2UvuwwVxOyU1uPqJcBNnr2yt3x3kw5+zDQ00z/pFntTXWm19m6BUtbkdwN2x
            Bn1P3P/mRTEaHxQr9RGg8Zjnix/Q6G7I5QIDAQAB
            -----END RSA PUBLIC KEY-----
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
  };
}
