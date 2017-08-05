let
  nixpkgs-lib = import <nixpkgs/lib>;
  lib = with lib; nixpkgs-lib // builtins // {

    evalSource = import ./eval-source.nix;

    git = import ./git.nix { inherit lib; };
    shell = import ./shell.nix { inherit lib; };
    types = nixpkgs-lib.types // import ./types.nix { inherit lib; };

    eq = x: y: x == y;
    ne = x: y: x != y;
    mod = x: y: x - y * (x / y);

    genid = import ./genid.nix { inherit lib; };
    genid_signed = x: ((lib.genid x) + 16777216) / 2;

    lpad = n: c: s:
      if lib.stringLength s < n
        then lib.lpad n c (c + s)
        else s;

    genAttrs' = names: f: listToAttrs (map f names);

    getAttrs = names: set:
      listToAttrs (map (name: nameValuePair name set.${name})
                       (filter (flip hasAttr set) names));

    setAttr = name: value: set: set // { ${name} = value; };

    test = re: x: isString x && testString re x;

    testString = re: x: match re x != null;

    toC = x: let
      type = typeOf x;
      reject = throw "cannot convert ${type}";
    in {
      list = "{ ${concatStringsSep ", " (map toC x)} }";
      null = "NULL";
      set = if isDerivation x then toJSON x else reject;
      string = toJSON x; # close enough
    }.${type} or reject;

    indent = replaceChars ["\n"] ["\n  "];

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
              else "${concatStringsSep ":" lhs}::${concatStringsSep ":" rhs}";

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
        a: toLower (group-zeros (drop-leading-zeros a));
  };
in

lib
