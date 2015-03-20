{ lib }:
with lib;
with builtins;
rec {

  # Use `term` to construct XML.
  #
  # Examples:
  #
  #   (term "bool" null null)
  #   (term "cool" null [])
  #   (term "fool" { hurr = "durr"; } null)
  #   (term "hool" null [
  #     (term "tool" null null)
  #   ])
  #
  # See `render` for how these get transformed into actuall XML documents.
  #
  term = name: attrs: content: {
    inherit name attrs content;
  };

  empty = term null null null;

  # Ref http://www.w3.org/TR/xml/#syntax
  #
  # Example:
  #
  #   (quote "<cheez!>")                 #===>   &lt;cheez!&gt;
  #
  quote = let
    sub = {
      "&" = "&amp;";
      "<" = "&lt;";
      ">" = "&gt;";
      "'" = "&apos;";
      "\"" = "&quot;";
    };
  in
    stringAsChars (c: sub.${c} or c);

  # Turn an XML element to an XML document string.
  doc = t:
    "<?xml version='1.0' encoding='UTF-8'?>${render t}";

  # Render an XML element to a string.
  #
  # Rendering `empty` yields the empty string.
  #
  # Examples:
  #
  #   (term "bool" null null)                 #===>   <bool/>
  #   (term "cool" null [])                   #===>   <cool></cool>
  #   (term "fool" { hurr = "durr"; } null)   #===>   <fool hurr="durr"/>
  #   (term "hool" null [
  #     (term "tool" null null)
  #   ])                                      #===>   <hool><tool/></hool>
  #
  render = let
    render-attrs = attrs:
      getAttr (typeOf attrs) {
        null = "";
        set = concatStrings (mapAttrsToList (n: v: " ${n}=\"${v}\"") attrs);
      };

    render-content = content:
      getAttr (typeOf content) {
        bool = toJSON content;
        int = toJSON content;
        list = concatMapStrings render content;
        string = content;
      };
  in
    { name, attrs, content }:
    if name == null
      then ""
      else let
        attrs' = render-attrs attrs;
        content' = render-content content;
      in
        if content == null
          then "<${name}${attrs'}/>"
          else "<${name}${attrs'}>${content'}</${name}>";
}
