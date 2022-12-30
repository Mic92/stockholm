# Commit Messages Guideline

Commits SHOULD have the following format:

```
<namespace?> <component>: <change>

<rationale>

(<reference-name>: <reference-id>)?
```

## `<namespace>`
Defines where the change took place. This can be omitted if the
namespace is `krebs`. Namespaces may be shortened to one to four characters (
lassulus -> lass, makefu -> make, tv -> tv, shared -> sha)

## `<component>`
Name of the component which was touched. `component` is
rather fuzzy and may mean different things, just choose what would fit best.

Here are a numbers of samples for defining the component:

* Change `gum` in `krebs/3modules/makefu/default.nix`: `gum: change ip`
* Change `prepare.sh` in `krebs/4libs/infest`: `infest: prepare stockholm ISO`
* Remove `concat` in `krebs/5pkgs`: `concat: RIP`, this commit may like some `<rationale>`
* Update `types` in `krebs/3modules`: `lib/types: add managed bool to host type`
* Change host `gum` in `makefu/1systems/gum`: `ma gum: add taskserver`
* Change `tinc` module in `krebs/3modules`: `tinc module: add option enableLegacy`

## `<rationale>`
Describe some trivia why the commit was done:
```
whatsupnix: init

Import from https://github.com/NixOS/nix/issues/443#issuecomment-296752535
```

## `<reference>`
Defines external resouces related to the commit:
```
Closes: #123533
CVE: CVE-2016-00001
URL: https://example.com/CVE-2016-00001
```

## Remarks
As a general rule of thumb you can check out: https://www.slideshare.net/TarinGamberini/commit-messages-goodpractices
Of course the pattern not always fits perfectly (for example for refactoring),
just apply some common sense and define a useful commit message,
like `refactor krebs.setuid`.


