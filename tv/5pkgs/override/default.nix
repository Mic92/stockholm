with import <stockholm/lib>;
self: super:

mapNixDir (path: import path self super) ./.
