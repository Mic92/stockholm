{-# LANGUAGE LambdaCase #-}
module Util.Shutdown
    ( sendShutdownEvent
    , handleShutdownEvent
    , shutdown
    )
  where

import Control.Monad
import Data.Monoid
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import XMonad
import qualified XMonad.StackSet as W

sendShutdownEvent :: IO ()
sendShutdownEvent = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    a <- internAtom dpy "XMONAD_SHUTDOWN" False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw a 32 0 currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False

handleShutdownEvent :: Event -> X All
handleShutdownEvent = \case
  ClientMessageEvent { ev_message_type = mt } -> do
    c <- (mt ==) <$> getAtom "XMONAD_SHUTDOWN"
    when c shutdown
    return (All c)
  _ ->
    return (All True)

shutdown :: X ()
shutdown = do
  broadcastMessage ReleaseResources
  io . flush =<< asks display
  let wsData = show . W.mapLayout show . windowset
      maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
      maybeShow (t, Left str) = Just (t, str)
      maybeShow _ = Nothing
      extState =
        return . show . catMaybes . map maybeShow . Map.toList . extensibleState
  s <- gets (\s -> (wsData s : extState s))
  _ <- io $ do
    path <- getEnv "XMONAD_STATE"
    writeFile path (concatMap (++"\n") s)
    exitSuccess
  return ()
