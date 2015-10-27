module Util.Debunk
    ( printToErrors
    ) where

import XMonad
import System.FilePath ( (</>) )
import Control.Exception ( bracket )
import System.IO ( hPrint, stderr, openFile, hClose, IOMode( AppendMode ) )


printToErrors x = do
    dir <- getXMonadDir
    let base = dir </> "xmonad"
        err  = base ++ ".errors"
    bracket (openFile err AppendMode) hClose $ \h -> hPrint h x

