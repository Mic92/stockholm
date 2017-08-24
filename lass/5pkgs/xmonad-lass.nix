{ pkgs, ... }:
pkgs.writeHaskell "xmonad-lass" {
  executables.xmonad = {
    extra-depends = [
      "containers"
      "unix"
      "X11"
      "xmonad"
      "xmonad-contrib"
      "xmonad-stockholm"
    ];
    text = /* haskell */ ''
{-# LANGUAGE DeriveDataTypeable #-} -- for XS
{-# LANGUAGE FlexibleContexts #-} -- for xmonad'
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where
import XMonad

import qualified XMonad.StackSet as W
import Control.Exception
import Data.List (isInfixOf)
import System.Environment (getArgs, withArgs)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
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
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt (autoComplete, searchPredicate, XPConfig)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBringCopy)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.SimpleFloat (simpleFloat)

import XMonad.Stockholm.Shutdown

urxvtcPath :: FilePath
urxvtcPath = "${pkgs.rxvt_unicode}/bin/urxvtc"

myFont :: String
myFont = "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"

main :: IO ()
main = getArgs >>= \case
    ["--shutdown"] -> sendShutdownEvent
    _ -> mainNoArgs

mainNoArgs :: IO ()
mainNoArgs = do
    xmonad'
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = urxvtcPath
            , modMask           = mod4Mask
            , layoutHook = smartBorders $ myLayoutHook
            , manageHook        = placeHook (smart (1,0)) <+> floatNextHook
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            , handleEventHook = handleShutdownEvent
            , workspaces        = [ "dashboard" ]
            } `additionalKeysP` myKeyMap

myLayoutHook = defLayout
  where
    defLayout = minimize $ ((avoidStruts $ Tall 1 (3/100) (1/2) ||| Full ||| Mirror (Tall 1 (3/100) (1/2))) ||| FixedColumn 2 80 80 1) ||| simpleFloat


xmonad' :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad' conf = do
    let path = "/tmp/xmonad.state"
    try (readFile path) >>= \case
        Right content -> do
            hPutStrLn stderr ("resuming from " ++ path)
            withArgs ("--resume" : lines content) (xmonad conf)
        Left e -> do
            hPutStrLn stderr (displaySomeException e)
            xmonad conf

displaySomeException :: SomeException -> String
displaySomeException = displayException


myKeyMap :: [([Char], X ())]
myKeyMap =
    [ ("M4-<F11>", spawn "${pkgs.i3lock}/bin/i3lock -i /var/lib/wallpaper/wallpaper -f")
    , ("M4-C-p", spawn "${pkgs.scrot}/bin/scrot ~/public_html/scrot.png")
    , ("M4-p", spawn "${pkgs.pass}/bin/passmenu --type")
    , ("M4-o", spawn "${pkgs.brain}/bin/brainmenu --type")
    , ("<XF86AudioRaiseVolume>", spawn "${pkgs.pulseaudioLight.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ +4%")
    , ("<XF86AudioLowerVolume>", spawn "${pkgs.pulseaudioLight.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ -4%")
    , ("<XF86MonBrightnessDown>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -time 0 -dec 1%")
    , ("<XF86MonBrightnessUp>",   spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -time 0 -inc 1")
    , ("<XF86Launch1>", gridselectWorkspace gridConfig W.view)
    , ("M4-C-k", spawn "${pkgs.xorg.xkill}/bin/xkill")

    , ("M4-a", focusUrgent)
    , ("M4-S-r", renameWorkspace    def)
    , ("M4-S-a", addWorkspacePrompt def)
    , ("M4-S-<Backspace>", removeEmptyWorkspace)
    , ("M4-S-c", kill1)
    , ("M4-<Esc>", toggleWS)
    , ("M4-S-<Enter>", spawn urxvtcPath)
    , ("M4-x", floatNext True >> spawn urxvtcPath)
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

    , ("M4-w", floatNext True >> spawn "${pkgs.copyq}/bin/copyq show")
    ]

forkFile :: FilePath -> [String] -> Maybe [(String, String)] -> X ()
forkFile path args env =
    xfork (executeFile path False args env) >> return ()

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
    { gs_cellwidth = 100
    , gs_cellheight = 30
    , gs_cellpadding = 2
    , gs_navigate = navNSearch
    , gs_font = myFont
    }
    '';
  };
}
