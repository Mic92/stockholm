{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Build where

import XMonad (Dimension)
import THEnv.JSON (getCompileEnvJSONExp)


myFont :: String
myFont =
  "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"

myScreenWidth :: Dimension
myScreenWidth =
  $(getCompileEnvJSONExp (id @Dimension) "XMONAD_BUILD_SCREEN_WIDTH")

myTermFontWidth :: Dimension
myTermFontWidth =
  $(getCompileEnvJSONExp (id @Dimension) "XMONAD_BUILD_TERM_FONT_WIDTH")

myTermPadding :: Dimension
myTermPadding =
  2
