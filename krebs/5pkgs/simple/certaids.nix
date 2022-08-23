{ pkgs }:

pkgs.write "certaids" {
  "/bin/cert2json".link = pkgs.writeDash "cert2json" ''
    # usage: cert2json < CERT > JSON
    set -efu

    ${pkgs.openssl}/bin/openssl crl2pkcs7 -nocrl -certfile /dev/stdin |
    ${pkgs.openssl}/bin/openssl pkcs7 -print_certs -text |
    ${pkgs.gawk}/bin/awk -F, -f ${pkgs.writeText "cert2json.awk" ''
      function abort(msg) {
        print(msg) > "/dev/stderr"
        exit 1
      }

      function toJSON(x,   type, ret) {
        type = typeof(x)
        switch (type) {
          case "array":
            if (isArray(x)) return arrayToJSON(x)
            if (isObject(x)) return objectToJSON(x)
            abort("cannot render array to JSON", x)
          case "number":
            return numberToJSON(x)
          case "string":
            return stringToJSON(x)
          case "strnum":
          case "unassigned":
          case "regexp":
          case "untyped":
          default:
            abort("cannot render type: " type)
        }
      }

      function isArray(x,   i, k) {
        i = 1
        for (k in x) {
          if (k != i++) return 0
          i++
        }
        return 1
      }

      function isObject(x,   k) {
        for (k in x) {
          if (typeof(k) != "string") return 0
        }
        return 1
      }

      function arrayToJSON(x,   k, ret) {
        ret = "["
        for (k in x) {
          ret=ret toJSON(x[k]) ","
        }
        sub(/,$/,"",ret)
        ret=ret "]"
        return ret
      }

      function objectToJSON(x,   k,ret) {
        ret = "{"
        for (k in x) {
          ret = ret toJSON(k) ":" toJSON(x[k]) ","
        }
        sub(/,$/, "", ret)
        ret = ret "}"
        return ret
      }

      function numberToJSON(x) {
        return x
      }

      function stringToJSON(x) {
        gsub(/\\/, "&&",x)
        gsub(/\n/, "\\n", x)
        return "\"" x "\""
      }

      $1 ~ /^ *(Subject|Issuer):/ {
        sub(/^ */, "")
        sub(/: */, ",")
        key=tolower($1)
        sub(/[^,]*,/, "")

        # Normalize separators between relative distinguished names.
        # [1]: RFC2253, 3. Parsing a String back to a Distinguished Name
        # TODO support any distinguished name
        gsub(/ *[;,] */, ",")

        for(i = 0; i <= NF; i++) {
          split($i, a, "=")
          cache[key][a[1]] = a[2]
        }
      }

      /BEGIN CERTIFICATE/,/END CERTIFICATE/{
        cache["certificate"] = cache["certificate"] $0 "\n"
      }

      /END CERTIFICATE/{
        print toJSON(cache)
        delete cache
      }
    ''}
  '';
}
