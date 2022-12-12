{-# LANGUAGE LambdaCase #-}

module Shutdown
    ( newShutdownEventHandler
    , shutdown
    )
  where

import Control.Applicative ((<|>), empty)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, guard, when)
import Data.Monoid (All(All))
import System.Directory (XdgDirectory(XdgData), createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory, getXdgDirectory)
import System.Exit (exitSuccess)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)
import XMonad hiding (getXMonadDataDir)


-- XXX this is for compatibility with both xmonad<0.17 and xmonad>=0.17
getXMonadDataDir :: IO String
getXMonadDataDir = xmEnvDir <|> xmDir <|> xdgDir
  where
    -- | Check for xmonad's environment variables first
    xmEnvDir :: IO String
    xmEnvDir =
        maybe empty pure =<< lookupEnv "XMONAD_DATA_DIR"

    -- | Check whether the config file or a build script is in the
    -- @~\/.xmonad@ directory
    xmDir :: IO String
    xmDir = do
        d <- getAppUserDataDirectory "xmonad"
        conf  <- doesFileExist $ d </> "xmonad.hs"
        build <- doesFileExist $ d </> "build"
        pid <- doesFileExist $ d </> "xmonad.pid"

        -- Place *everything* in ~/.xmonad if yes
        guard $ conf || build || pid
        pure d

    -- | Use XDG directories as a fallback
    xdgDir :: IO String
    xdgDir = do
        d <- getXdgDirectory XdgData "xmonad"
        d <$ createDirectoryIfMissing True d


newShutdownEventHandler :: IO (Event -> X All)
newShutdownEventHandler = do
    writeProcessIDToFile
    return handleShutdownEvent

handleShutdownEvent :: Event -> X All
handleShutdownEvent = \case
  ClientMessageEvent { ev_message_type = mt } -> do
    isShutdownEvent <- (mt ==) <$> getAtom "XMONAD_SHUTDOWN"
    when isShutdownEvent $ do
      broadcastMessage ReleaseResources
      writeStateToFile
      io exitSuccess >> return ()
    return (All (not isShutdownEvent))
  _ ->
    return (All True)

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

shutdown :: IO ()
shutdown = do
    pid <- readProcessIDFromFile
    sendShutdownEvent
    hPutStrLn stderr ("waiting for: " <> show pid)
    result <- tryIOError (waitProcess pid)
    if isSuccess result
      then hPutStrLn stderr ("result: " <> show result <> " [AKA success^_^]")
      else hPutStrLn stderr ("result: " <> show result)
  where
    isSuccess = either isDoesNotExistError (const False)

waitProcess :: ProcessID -> IO ()
waitProcess pid = forever (signalProcess nullSignal pid >> threadDelay 10000)

--
-- PID file stuff
--

getProcessIDFileName :: IO FilePath
getProcessIDFileName = (</> "xmonad.pid") <$> getXMonadDataDir

writeProcessIDToFile :: IO ()
writeProcessIDToFile = do
    pidFileName <- getProcessIDFileName
    pid <- getProcessID
    writeFile pidFileName (show pid)

readProcessIDFromFile :: IO ProcessID
readProcessIDFromFile = do
    pidFileName <- getProcessIDFileName
    read <$> readFile pidFileName
