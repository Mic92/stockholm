{ lib, ... }:
let
  nixpkgs-lib = lib;
  stockholm.lib = with stockholm.lib; nixpkgs-lib // builtins // {

    evalModulesConfig = modules: let
      eval = evalModules {
        inherit modules;
      };
    in filterAttrsRecursive (name: _: !hasPrefix "_" name) eval.config;

    evalSource = import ./eval-source.nix;

    evalSubmodule = submodule: modules: let
      prefix = ["evalSubmodule"];
    in evalModulesConfig [
      {
        options = removeAttrs (submodule.getSubOptions prefix) ["_module"];
        imports = modules;
      }
    ];

    git = import ./git.nix { inherit (stockholm) lib; };
    haskell = import ./haskell.nix { inherit (stockholm) lib; };
    krebs = import ./krebs stockholm.lib;
    shell = import ./shell.nix { inherit (stockholm) lib; };
    systemd = {
      encodeName = replaceStrings ["/"] ["\\x2f"];
    };
    types = nixpkgs-lib.types // import ./types.nix { lib = stockholm.lib; };
    uri = import ./uri.nix { inherit (stockholm) lib; };
    xml = import ./xml.nix { inherit (stockholm) lib; };

    # compose a list of functions to be applied from left to right, i.e.
    # compose :: [ (xm -> xn) ... (x1 -> x2) (x0 -> x1) ] -> x0 -> xn
    compose = foldl' (f: g: x: f (g x)) id;

    eq = x: y: x == y;
    ne = x: y: x != y;
    mod = x: y: x - y * (x / y);

    on = b: u: x: y: b (u x) (u y);

    genid = stockholm.lib.genid_uint32; # TODO remove
    genid_uint31 = x: ((stockholm.lib.genid_uint32 x) + 16777216) / 2;
    genid_uint32 = import ./genid.nix { lib = stockholm.lib; };

    hexchars = stringToCharacters "0123456789abcdef";

    lpad = n: c: s:
      if lib.stringLength s < n
        then stockholm.lib.lpad n c (c + s)
        else s;

    genAttrs' = names: f: listToAttrs (map f names);

    getAttrs = names: set:
      listToAttrs (map (name: nameValuePair name set.${name})
                       (filter (flip hasAttr set) names));

    maybeHead = x: if isList x && length x > 0 then head x else null;

    packageName = pkg:
      pkg.pname or (parseDrvName pkg.name).name;

    test = re: x: isString x && testString re x;

    testString = re: x: match re x != null;

    toC = x: let
      type = typeOf x;
      reject = throw "cannot convert ${type}";
    in {
      int = toJSON x; # close enough
      list = "{ ${concatStringsSep ", " (map toC x)} }";
      null = "NULL";
      set = if isDerivation x then toJSON x else reject;
      string = toJSON x; # close enough
    }.${type} or reject;

    indent = replaceStrings ["\n"] ["\n  "];

    stripAttr = converge (filterAttrsRecursive (n: v: v != {} && v != null));

    mapNixDir = f: x: {
      list = foldl' mergeAttrs {} (map (mapNixDir1 f) x);
      path = mapNixDir1 f x;
    }.${typeOf x};

    mapNixDir1 = f: dirPath:
      let
        toPackageName = name:
          if test "^[0-9].*" name then "_${name}" else name;
      in
      listToAttrs
        (map
          (relPath: let
            name = removeSuffix ".nix" relPath;
            path = dirPath + "/${relPath}";
          in
            nameValuePair (toPackageName name) (f path))
          (attrNames
            (filterAttrs isNixDirEntry (readDir dirPath))));

    isNixDirEntry = name: type:
      (type == "regular" && hasSuffix ".nix" name && name != "default.nix") ||
      (type == "directory" && !hasPrefix "." name);

    # https://tools.ietf.org/html/rfc5952
    normalize-ip6-addr =
      let
        max-run-0 =
          let
            both = v: { off = v; pos = v; };
            gt = a: b: a.pos - a.off > b.pos - b.off;

            chkmax = ctx: {
              cur = both (ctx.cur.pos + 1);
              max = if gt ctx.cur ctx.max then ctx.cur else ctx.max;
            };

            incpos = ctx: recursiveUpdate ctx {
              cur.pos = ctx.cur.pos + 1;
            };

            f = ctx: blk: (if blk == "0" then incpos else chkmax) ctx;
            z = { cur = both 0; max = both 0; };
          in
            blks: (chkmax (foldl' f z blks)).max;

        group-zeros = a:
          let
            blks = splitString ":" a;
            max = max-run-0 blks;
            lhs = take max.off blks;
            rhs = drop max.pos blks;
          in
            if max.pos == 0
              then a
              else let
                sep =
                  if 8 - (length lhs + length rhs) == 1
                    then ":0:"
                    else "::";
              in
                "${concatStringsSep ":" lhs}${sep}${concatStringsSep ":" rhs}";

        drop-leading-zeros =
          let
            f = block:
              let
                res = match "0*(.+)" block;
              in
                if res == null
                  then block # empty block
                  else elemAt res 0;
          in
            a: concatStringsSep ":" (map f (splitString ":" a));
      in
        a:
          toLower
            (if test ".*::.*" a
              then a
              else group-zeros (drop-leading-zeros a));

    hashToLength = n: s: substring 0 n (hashString "sha256" s);

    dropLast = n: xs: reverseList (drop n (reverseList xs));
    takeLast = n: xs: reverseList (take n (reverseList xs));

    # Split string into list of chunks where each chunk is at most n chars long.
    # The leftmost chunk might shorter.
    # Example: stringToGroupsOf "123456" -> ["12" "3456"]
    stringToGroupsOf = n: s: let
      acc =
        foldl'
          (acc: c: if stringLength acc.chunk < n then {
            chunk = acc.chunk + c;
            chunks = acc.chunks;
          } else {
            chunk = c;
            chunks = acc.chunks ++ [acc.chunk];
          })
          {
            chunk = "";
            chunks = [];
          }
          (stringToCharacters s);
    in
      filter (x: x != []) ([acc.chunk] ++ acc.chunks);

    # Filter adjacent duplicate elements.
    uniq = uniqBy eq;

    # Filter adjacent duplicate elements determined via the given function.
    uniqBy = cmp: let
      f = a: s:
        if length s == 0 then
          []
        else let
          b = head s;
        in
          if cmp a b then
            f b (tail s)
          else
            [b] ++ f b (tail s);
    in
      s:
        if length s == 0 then
          []
        else let
          b = head s;
        in
          [b] ++ f b (tail s);

    warnOldVersion = oldName: newName:
      if compareVersions oldName newName != -1 then
        trace "Upstream `${oldName}' gets overridden by `${newName}'." newName
      else
        newName;
  };
in

stockholm.lib
// { lib = stockholm.lib; }

