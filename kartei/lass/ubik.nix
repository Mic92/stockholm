{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.12";
      ip6.addr = r6 "0b1c";
      aliases = [
        "ubik.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAnWJKDrDmmGZbwVeaBhvOdTR4nsumo1yzOR2Iu+SMTOH6fbgJM5cW
        WtlgPhrdOMrBYR956SBiBNkvsdczRrOF7F6hvXyDwwoGdWGsZXzaTMJlNAYjP5Y4
        fbJlDq8/QV/SvVFGeu4XP3g2yuU/aNu/4FkU4jlysX+8wo9qGpIFPLpLvqfuU247
        jHCatNzHfLK60fx7yt57iDhuX2plyFfQVX7xPTxudfGZKD7rEDEnKX4Ghd5dUkOA
        z0lr0B1AOrkZgrnajU0ZmkjnNy8lrylCWDOnEPhJdao53gL4XFmUcZaR4uFsWuS7
        V1VM+VivuMTAXRUnJScyLap2mo6dcr9h11kas70c/R7tI2pGmxlNk9t2uYy/jQnC
        WmyzNCcqpPSfKikx5sRVAVIuv2wtAKYDuZg+1D4YEfeklA0+ZZlHO43NnRnIoKeO
        Za0SNUE6vtd/EPoiifMkOWtHaO0LppgOxMTk8OgUxR6dcTmbuL0Roz3aY0rSW3EG
        +li3yjS3YAtMtvhQwuqooVrkBFrcGQLjTnAfCeUHbCjZidGAHnqhESA+Aj+LKx32
        0ALQY439xAs6Vf3rICs93cO4Yxa8W1F5sHE6ANOGU+jCmSkCWI2hdHGbckD3L0AQ
        NBJ+jyXm0kFfVgqRS2i17JPz2ZZxhAHw3KH13Ef1KI4tMdzCvFSayW0CAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "BcbZOID7dipWNH0/uowqCF7Ivqm4QktMoz11Yv249tG";
    };
    wiregrill = {
      ip6.addr = w6 "0b1c";
      aliases = [
        "ubik.w"
      ];
      wireguard.pubkey = ''
        JakWwg7Rq76jjzLFWPBQJPpzRHbIEbb46VLsSUOKI2I=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlqW8zqJpjbva0NTty9Ex7R/Jk2emDxHJNpaM3WPt5L";
}
