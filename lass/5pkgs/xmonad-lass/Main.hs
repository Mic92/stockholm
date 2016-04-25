{-# LANGUAGE DeriveDataTypeable #-} -- for XS
{-# LANGUAGE FlexibleContexts #-} -- for xmonad'
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where
import XMonad

import qualified XMonad.StackSet as W
import Control.Exception
import Data.List (isInfixOf)
import System.Environment (getArgs, withArgs, getEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readEither)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace, removeEmptyWorkspace)
import XMonad.Actions.DynamicWorkspaces (withWorkspace)
import XMonad.Actions.GridSelect (GSConfig(..), gridselectWorkspace, navNSearch)
import XMonad.Hooks.FloatNext (floatNext)
import XMonad.Hooks.FloatNext (floatNextHook)
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.UrgencyHook (SpawnUrgencyHook(..), withUrgencyHook)
import XMonad.Layout.FixedColumn (FixedColumn(..))
import XMonad.Layout.Minimize (minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Operations (withFocused)
import XMonad.Prompt (autoComplete, searchPredicate, XPConfig)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBringCopy)
import XMonad.Stockholm.Shutdown (sendShutdownEvent, handleShutdownEvent)
import XMonad.Util.EZConfig (additionalKeysP)


myTerm :: String
myTerm = "urxvtc"

myRootTerm :: String
myRootTerm = "urxvtc -name root-urxvt -e su -"

myFont :: String
myFont = "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"

main :: IO ()
main = getArgs >>= \case
    ["--shutdown"] -> sendShutdownEvent
    _ -> mainNoArgs

mainNoArgs :: IO ()
mainNoArgs = do
    workspaces0 <- getWorkspaces0
    xmonad'
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = myTerm
            , modMask           = mod4Mask
            , workspaces        = workspaces0
            , layoutHook = smartBorders $ myLayoutHook
            , manageHook        = placeHook (smart (1,0)) <+> floatNextHook
            , startupHook       = spawn "echo emit XMonadStartup"
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            , handleEventHook = handleShutdownEvent
            } `additionalKeysP` myKeyMap

myLayoutHook = defLayout
  where
    defLayout = minimize $ ((avoidStruts $ Tall 1 (3/100) (1/2) ||| Full ||| Mirror (Tall 1 (3/100) (1/2))) ||| FixedColumn 2 80 80 1)


xmonad' :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad' conf = do
    path <- getEnv "XMONAD_STATE"
    try (readFile path) >>= \case
        Right content -> do
            hPutStrLn stderr ("resuming from " ++ path ++ "; state = " ++ show content)
            withArgs ("--resume" : lines content) (xmonad conf)
        Left e -> do
            hPutStrLn stderr (displaySomeException e)
            xmonad conf

getWorkspaces0 :: IO [String]
getWorkspaces0 =
    try (getEnv "XMONAD_WORKSPACES0_FILE") >>= \case
      Left e -> warn (displaySomeException e)
      Right p -> try (readFile p) >>= \case
        Left e -> warn (displaySomeException e)
        Right x -> case readEither x of
          Left e -> warn e
          Right y -> return y
  where
    warn msg = hPutStrLn stderr ("getWorkspaces0: " ++ msg) >> return []

displaySomeException :: SomeException -> String
displaySomeException = displayException


myKeyMap :: [([Char], X ())]
myKeyMap =
    [ ("M4-<F11>", spawn "i3lock -i /tmp/wallpaper.png -f")
    , ("M4-p", spawn "passmenu --type")
    , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +4%")
    , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -4%")
    , ("<XF86AudioMute>", spawn "pactl -- set-sink-mute 0 toggle")
    , ("<XF86AudioMicMute>", spawn "pactl -- set-source-mute 1 toggle")
    , ("<XF86Launch1>", gridselectWorkspace gridConfig W.view)

    , ("M4-a", focusUrgent)
    , ("M4-S-r", renameWorkspace    def)
    , ("M4-S-a", addWorkspacePrompt def)
    , ("M4-S-<Backspace>", removeEmptyWorkspace)
    , ("M4-S-c", kill1)
    , ("M4-<Esc>", toggleWS)
    , ("M4-S-<Enter>", spawn myTerm)
    , ("M4-x", floatNext True >> spawn myTerm)
    , ("M4-f", floatNext True)
    , ("M4-b", sendMessage ToggleStruts)

    , ("M4-v", withWorkspace autoXPConfig (windows . W.view))
    , ("M4-S-v", withWorkspace autoXPConfig (windows . W.shift))
    , ("M4-C-v", withWorkspace autoXPConfig (windows . copy))

    , ("M4-m", withFocused minimizeWindow)
    , ("M4-S-m", sendMessage RestoreNextMinimizedWin)

    , ("M4-q", windowPromptGoto infixAutoXPConfig)
    , ("M4-C-q", windowPromptBringCopy infixAutoXPConfig)

    , ("M4-S-q", return ())
    ]

autoXPConfig :: XPConfig
autoXPConfig = def
    { autoComplete = Just 5000
    }

infixAutoXPConfig :: XPConfig
infixAutoXPConfig = autoXPConfig
    { searchPredicate = isInfixOf
    }

gridConfig :: GSConfig WorkspaceId
gridConfig = def
    { gs_cellwidth = 50
    , gs_cellheight = 50
    , gs_cellpadding = 2
    , gs_navigate = navNSearch
    , gs_font = myFont
    }
