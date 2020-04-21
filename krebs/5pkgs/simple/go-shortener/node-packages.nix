# This file has been generated by node2nix 1.7.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "denque-1.4.1" = {
      name = "denque";
      packageName = "denque";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/denque/-/denque-1.4.1.tgz";
        sha512 = "OfzPuSZKGcgr96rf1oODnfjqBFmr1DVoc/TrItj3Ohe0Ah1C5WX5Baquw/9U9KovnQ88EqmJbD66rKYUQYN1tQ==";
      };
    };
    "redis-commands-1.5.0" = {
      name = "redis-commands";
      packageName = "redis-commands";
      version = "1.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/redis-commands/-/redis-commands-1.5.0.tgz";
        sha512 = "6KxamqpZ468MeQC3bkWmCB1fp56XL64D4Kf0zJSwDZbVLLm7KFkoIcHrgRvQ+sk8dnhySs7+yBg94yIkAK7aJg==";
      };
    };
    "redis-errors-1.2.0" = {
      name = "redis-errors";
      packageName = "redis-errors";
      version = "1.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/redis-errors/-/redis-errors-1.2.0.tgz";
        sha1 = "eb62d2adb15e4eaf4610c04afe1529384250abad";
      };
    };
    "redis-parser-3.0.0" = {
      name = "redis-parser";
      packageName = "redis-parser";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/redis-parser/-/redis-parser-3.0.0.tgz";
        sha1 = "b66d828cdcafe6b4b8a428a7def4c6bcac31c8b4";
      };
    };
  };
in
{
  formidable = nodeEnv.buildNodePackage {
    name = "formidable";
    packageName = "formidable";
    version = "1.2.2";
    src = fetchurl {
      url = "https://registry.npmjs.org/formidable/-/formidable-1.2.2.tgz";
      sha512 = "V8gLm+41I/8kguQ4/o1D3RIHRmhYFG4pnNyonvua+40rqcEmT4+V71yaZ3B457xbbgCsCfjSPi65u/W6vK1U5Q==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "A node.js module for parsing form data, especially file uploads.";
      homepage = https://github.com/node-formidable/formidable;
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  redis = nodeEnv.buildNodePackage {
    name = "redis";
    packageName = "redis";
    version = "3.0.2";
    src = fetchurl {
      url = "https://registry.npmjs.org/redis/-/redis-3.0.2.tgz";
      sha512 = "PNhLCrjU6vKVuMOyFu7oSP296mwBkcE6lrAjruBYG5LgdSqtRBoVQIylrMyVZD/lkF24RSNNatzvYag6HRBHjQ==";
    };
    dependencies = [
      sources."denque-1.4.1"
      sources."redis-commands-1.5.0"
      sources."redis-errors-1.2.0"
      sources."redis-parser-3.0.0"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "A high performance Redis client.";
      homepage = https://github.com/NodeRedis/node-redis;
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}