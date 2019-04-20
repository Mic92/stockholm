with import <stockholm/lib>;
{ config, ... }: let

  certFile = config.environment.etc."ssl/certs/ca-certificates.crt".source;

in {

  environment.variables = flip genAttrs (_: toString certFile) [
    "CURL_CA_BUNDLE"
    "GIT_SSL_CAINFO"
    "SSL_CERT_FILE"
  ];

}
