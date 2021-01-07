{ lib }:
with lib;
with builtins;
rec {
  # Regular expression to match URIs per RFC3986
  # From: # http://jmrware.com/articles/2009/uri_regexp/URI_regex.html#uri-40
  native-regex = ''
    # RFC-3986 URI component:  URI
    [A-Za-z][A-Za-z0-9+\-.]* :                                      # scheme ":"
    (?: //                                                          # hier-part
      (?: (?:[A-Za-z0-9\-._~!$&'()*+,;=:]|%[0-9A-Fa-f]{2})* @)?
      (?:
        \[
        (?:
          (?:
            (?:                                                    (?:[0-9A-Fa-f]{1,4}:){6}
            |                                                   :: (?:[0-9A-Fa-f]{1,4}:){5}
            | (?:                            [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:){4}
            | (?: (?:[0-9A-Fa-f]{1,4}:){0,1} [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:){3}
            | (?: (?:[0-9A-Fa-f]{1,4}:){0,2} [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:){2}
            | (?: (?:[0-9A-Fa-f]{1,4}:){0,3} [0-9A-Fa-f]{1,4})? ::    [0-9A-Fa-f]{1,4}:
            | (?: (?:[0-9A-Fa-f]{1,4}:){0,4} [0-9A-Fa-f]{1,4})? ::
            ) (?:
                [0-9A-Fa-f]{1,4} : [0-9A-Fa-f]{1,4}
              | (?: (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?) \.){3}
                    (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
              )
          |   (?: (?:[0-9A-Fa-f]{1,4}:){0,5} [0-9A-Fa-f]{1,4})? ::    [0-9A-Fa-f]{1,4}
          |   (?: (?:[0-9A-Fa-f]{1,4}:){0,6} [0-9A-Fa-f]{1,4})? ::
          )
        | [Vv][0-9A-Fa-f]+\.[A-Za-z0-9\-._~!$&'()*+,;=:]+
        )
        \]
      | (?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
           (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
      | (?:[A-Za-z0-9\-._~!$&'()*+,;=]|%[0-9A-Fa-f]{2})*
      )
      (?: : [0-9]* )?
      (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
    | /
      (?:    (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+
        (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
      )?
    |        (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+
        (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
    |
    )
    (?:\? (?:[A-Za-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9A-Fa-f]{2})* )?   # [ "?" query ]
    (?:\# (?:[A-Za-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9A-Fa-f]{2})* )?   # [ "#" fragment ]
  '';

  posix-extended-regex =
    let
      removeComment = s:
        elemAt (match "^((\\\\#|[^#])*)(#.*)?$" s) 0;

      removeWhitespace =
        replaceStrings [" "] [""];

      moveDashToEndOfCharacterClass = s:
        let
          result = match "(.*)\\\\-([^]]+)(].*)" s;
          s' = elemAt result 0 + elemAt result 1 + "-" + elemAt result 2;
        in
          if result != null then
            moveDashToEndOfCharacterClass s'
          else
            s;
    in
      concatStrings
        (foldl' (a: f: map f a) (splitString "\n" native-regex) [
          removeComment
          moveDashToEndOfCharacterClass
          (replaceStrings ["(?:"] ["("])
          removeWhitespace
        ]);
}
