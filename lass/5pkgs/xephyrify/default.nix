{ writeDashBin, writeHaskell, coreutils, xorg, virtualgl, ... }:

let

  minimalXmonad = writeHaskell "minimalXmonad" {
    executables.xmonad = {
      extra-depends = [
        "containers"
        "xmonad"
      ];
      text = /* haskell */ ''
        module Main where
        import XMonad
        import qualified Data.Map as Map

        main :: IO ()
        main = do
          xmonad def
            { workspaces = [ "1" ]
            , layoutHook = myLayoutHook
            , keys = myKeys
            , normalBorderColor  = "#000000"
            , focusedBorderColor = "#000000"
            }

        myLayoutHook = Full
        myKeys _ = Map.fromList []
      '';
    };
  };

in writeDashBin "xephyrify" ''
  NDISPLAY=:$(${coreutils}/bin/shuf -i 100-65536 -n 1)
  echo "using DISPLAY $NDISPLAY"
  ${xorg.xorgserver}/bin/Xephyr -br -ac -reset -terminate -resizeable $NDISPLAY &
  XEPHYR_PID=$!
  DISPLAY=$NDISPLAY ${minimalXmonad}/bin/xmonad &
  XMONAD_PID=$!
  DISPLAY=$NDISPLAY ${virtualgl}/bin/vglrun "$@"
  kill $XMONAD_PID
  kill $XEPHYR_PID
''
