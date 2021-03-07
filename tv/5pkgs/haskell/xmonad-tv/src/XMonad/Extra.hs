module XMonad.Extra where

import XMonad
import qualified Data.Map as Map
import qualified XMonad.StackSet as W


isFloating :: Window -> WindowSet -> Bool
isFloating w =
    Map.member w . W.floating

isFloatingX :: Window -> X Bool
isFloatingX w =
    isFloating w <$> gets windowset
