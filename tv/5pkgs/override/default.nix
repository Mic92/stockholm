with import ../../../lib;
self: super:

mapNixDir (path: import path self super) ./.
