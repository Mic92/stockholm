{ pkgs }:

/* gitignore - Filter for intentionally untracked lines or blocks of code

This is a filter that allows specifying intentionally untracked lines and
blocks of code that Git should ignore.

Example:

  int main(void) {
    printf("I would never say derp.\n");
    //#gitignore-begin
    printf("DERP!\n");
    //#gitignore-end
    printf("DERP!\n"); //#gitignore
    return 0;
  }

Installation:

  Define a filter, e.g. in ~/.config/git/config[1]:

      [filter "gitignore"]
        clean = gitignore
        smudge = cat

  Assing that filter to some paths, e.g. in ~/.config/git/attributes[2]:

      *.hs filter=gitignore
      *.c filter=gitignore
      ...

  [1]: For more information about defining filters see git-config(1).
  [2]: For more information about assigning filters see gitattributes(5).
*/

pkgs.execBin "gitignore" {
  filename = "${pkgs.gnused}/bin/sed";
  argv = [
    "gitignore"
    /* sed */ ''
      /#gitignore-begin/,/#gitignore-end/d
      /#gitignore/d
    ''
  ];
}
