lib:
with lib;
let {
  body = netname: subnetname: suffixSpec: rec {
    address = let
      suffix' = prependZeros suffixLength suffix;
    in
      normalize-ip6-addr
        (checkAddress addressLength (joinAddress subnetPrefix suffix'));
    addressCIDR = "${address}/${toString addressLength}";
    addressLength = 128;

    inherit netname;
    netCIDR = "${netAddress}/${toString netPrefixLength}";
    netAddress =
      normalize-ip6-addr (appendZeros addressLength netPrefix);
    netHash = toString {
      retiolum = 0;
      wirelum = 1;
    }.${netname};
    netPrefix = "42:${netHash}";
    netPrefixLength = {
      retiolum = 32;
      wirelum = 32;
    }.${netname};

    inherit subnetname;
    subnetCIDR = "${subnetAddress}/${toString subnetPrefixLength}";
    subnetAddress =
      normalize-ip6-addr (appendZeros addressLength subnetPrefix);
    subnetHash = hashToLength 4 subnetname;
    subnetPrefix = joinAddress netPrefix subnetHash;
    subnetPrefixLength = netPrefixLength + 16;

    suffix = getAttr (typeOf suffixSpec) {
      set =
        concatStringsSep
          ":"
          (stringToGroupsOf
            4
            (hashToLength (suffixLength / 4) suffixSpec.hostName));
      string = suffixSpec;
    };
    suffixLength = addressLength - subnetPrefixLength;
  };

  appendZeros = n: s: let
    n' = n / 16;
    zeroCount = n' - length parsedaddr;
    parsedaddr = parseAddress s;
  in
    formatAddress (parsedaddr ++ map (const "0") (range 1 zeroCount));

  prependZeros = n: s: let
    n' = n / 16;
    zeroCount = n' - length parsedaddr;
    parsedaddr = parseAddress s;
  in
    formatAddress (map (const "0") (range 1 zeroCount) ++ parsedaddr);

  hasEmptyPrefix = xs: take 2 xs == ["" ""];
  hasEmptySuffix = xs: takeLast 2 xs == ["" ""];
  hasEmptyInfix = xs: any (x: x == "") (trimEmpty 2 xs);

  hasEmptyGroup = xs:
    any (p: p xs) [hasEmptyPrefix hasEmptyInfix hasEmptySuffix];

  ltrimEmpty = n: xs: if hasEmptyPrefix xs then drop n xs else xs;
  rtrimEmpty = n: xs: if hasEmptySuffix xs then dropLast n xs else xs;
  trimEmpty = n: xs: rtrimEmpty n (ltrimEmpty n xs);

  parseAddress = splitString ":";
  formatAddress = concatStringsSep ":";

  check = s: c: if !c then throw "${s}" else true;

  checkAddress = maxaddrlen: addr: let
    parsedaddr = parseAddress addr;
    normalizedaddr = trimEmpty 1 parsedaddr;
  in
    assert (check "address malformed; lone leading colon: ${addr}" (
      head parsedaddr == "" -> tail (take 2 parsedaddr) == ""
    ));
    assert (check "address malformed; lone trailing colon ${addr}" (
      last parsedaddr == "" -> head (takeLast 2 parsedaddr) == ""
    ));
    assert (check "address malformed; too many successive colons: ${addr}" (
      length (filter (x: x == "") normalizedaddr) > 1 -> addr == [""]
    ));
    assert (check "address malformed: ${addr}" (
      all (test "[0-9a-f]{0,4}") parsedaddr
    ));
    assert (check "address is too long: ${addr}" (
      length normalizedaddr * 16 <= maxaddrlen
    ));
    addr;

  joinAddress = prefix: suffix: let
    parsedPrefix = parseAddress prefix;
    parsedSuffix = parseAddress suffix;
    normalizePrefix = rtrimEmpty 2 parsedPrefix;
    normalizeSuffix = ltrimEmpty 2 parsedSuffix;
    delimiter =
      optional (length (normalizePrefix ++ normalizeSuffix) < 8 &&
                (hasEmptySuffix parsedPrefix || hasEmptyPrefix parsedSuffix))
               "";
  in
    formatAddress (normalizePrefix ++ delimiter ++ normalizeSuffix);
}
