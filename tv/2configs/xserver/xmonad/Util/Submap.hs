-- This module is based on Jason Creighton's XMonad.Actions.Submap

module Util.Submap
    ( submapString
    ) where

import Data.Bits
import XMonad hiding (keys)
import qualified Data.Map as M
import Control.Monad.Fix (fix)


-- | Like 'XMonad.Actions.Submap.submapDefault', but provides the looked up string to the default action.
submapString :: (String -> X ()) -> M.Map (KeyMask, KeySym) (X ()) -> X ()
submapString def keys = do
    XConf { theRoot = root, display = d } <- ask

    (m, s, str) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else do
                (mbKeysym, str) <- lookupString (asKeyEvent p)
                return (m, keysym, str)

    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    maybe (def str) id (M.lookup (m', s) keys)
