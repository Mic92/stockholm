{ config, pkgs, ... }:
pkgs.writeHaskellPackage "xmonad-lass" {
  executables.xmonad = {
    extra-depends = [
      "containers"
      "extra"
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
import Control.Monad.Extra (whenJustM)
import Data.List (isInfixOf)
import Data.Monoid (Endo)
import System.Environment (getArgs, lookupEnv)
import System.Posix.Process (executeFile)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace, removeEmptyWorkspace)
import XMonad.Actions.DynamicWorkspaces (withWorkspace)
import XMonad.Actions.GridSelect (GSConfig(..), gridselectWorkspace, navNSearch)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FloatNext (floatNext)
import XMonad.Hooks.FloatNext (floatNextHook)
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, UrgencyHook(..))
import XMonad.Layout.FixedColumn (FixedColumn(..))
import XMonad.Layout.Minimize (minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Prompt (autoComplete, font, searchPredicate, XPConfig)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBringCopy)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)

import XMonad.Stockholm.Shutdown (handleShutdownEvent, sendShutdownEvent)
import XMonad.Stockholm.Pager (defaultWindowColors, pager, MatchMethod(MatchPrefix), PagerConfig(..))

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "${pkgs.libnotify}/bin/notify-send" [show name, "workspace " ++ idx]

myTerm :: FilePath
myTerm = "${pkgs.rxvt_unicode_with-plugins}/bin/urxvtc"

myFont :: String
myFont = "${config.lass.fonts.regular}"

main :: IO ()
main = getArgs >>= \case
  ["--shutdown"] -> sendShutdownEvent
  _ -> main'

main' :: IO ()
main' = do
    xmonad $ ewmh
        $ withUrgencyHook LibNotifyUrgencyHook
        $ def
            { terminal           = myTerm
            , modMask            = mod4Mask
            , layoutHook         = smartBorders $ myLayoutHook
            , manageHook         = placeHook (smart (1,0)) <+> floatNextHook <+> floatHooks
            , startupHook =
                whenJustM (liftIO (lookupEnv "XMONAD_STARTUP_HOOK"))
                          (\path -> forkFile path [] Nothing)
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#ff0000"
            , handleEventHook    = handleShutdownEvent
            , workspaces         = [ "dashboard", "sys", "wp" ]
            } `additionalKeysP` myKeyMap

myLayoutHook = defLayout
  where
    defLayout = minimize $ ((avoidStruts $ Mirror (Tall 1 (3/100) (1/2))) ||| Full ||| FixedColumn 2 80 80 1 ||| Tall 1 (3/100) (1/2) ||| simplestFloat)

floatHooks :: Query (Endo WindowSet)
floatHooks = composeAll . concat $
    [ [ title =? t --> doFloat | t <- myTitleFloats]
    , [ className =? c --> doFloat | c <- myClassFloats ] ]
  where
      myTitleFloats = []
      myClassFloats = ["Pinentry"] -- for gpg passphrase entry


myKeyMap :: [([Char], X ())]
myKeyMap =
    [ ("M4-<F11>", spawn "${config.lass.screenlock.command}")
    , ("M4-C-p", spawn "${pkgs.scrot}/bin/scrot ~/public_html/scrot.png")
    , ("M4-p", spawn "${pkgs.pass}/bin/passmenu --type")
    , ("M4-o", spawn "${pkgs.brain}/bin/brainmenu --type")
    , ("M4-i", spawn "${pkgs.dpass}/bin/dpassmenu --type")

    , ("<XF86AudioMute>", spawn "${pkgs.pulseaudioLight.out}/bin/pactl -- set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioRaiseVolume>", spawn "${pkgs.pulseaudioLight.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ +4%")
    , ("<XF86AudioLowerVolume>", spawn "${pkgs.pulseaudioLight.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ -4%")
    , ("<XF86MonBrightnessDown>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -time 0 -dec 1%")
    , ("<XF86MonBrightnessUp>",   spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -time 0 -inc 1")
    , ("<XF86Launch1>", gridselectWorkspace gridConfig W.view)
    , ("M4-C-k", spawn "${pkgs.xorg.xkill}/bin/xkill")

    , ("M4-a", focusUrgent)
    , ("M4-S-r", renameWorkspace    myXPConfig)
    , ("M4-S-a", addWorkspacePrompt myXPConfig)
    , ("M4-S-<Backspace>", removeEmptyWorkspace)
    , ("M4-S-c", kill1)
    , ("M4-<Esc>", toggleWS)
    , ("M4-S-<Enter>", spawn myTerm)
    , ("M4-x", floatNext True >> spawn myTerm)
    , ("M4-c", floatNext True >> spawn "${pkgs.termite}/bin/termite")
    , ("M4-f", floatNext True)
    , ("M4-b", sendMessage ToggleStruts)

    , ("M4-v", gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.view) )
    , ("M4-S-v", gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ("M4-C-v", withWorkspace autoXPConfig (windows . copy))

    , ("M4-m", withFocused minimizeWindow)
    , ("M4-S-m", sendMessage RestoreNextMinimizedWin)

    , ("M4-q", windowPromptGoto infixAutoXPConfig)
    , ("M4-C-q", windowPromptBringCopy infixAutoXPConfig)

    , ("M4-S-q", return ())

    , ("M4-d", floatNext True >> spawn "${pkgs.copyq}/bin/copyq show")

    , ("M4-<F4>", spawn "${pkgs.writeDash "nm-dmenu" ''
      export PATH=$PATH:${pkgs.dmenu}/bin:${pkgs.networkmanagerapplet}/bin
      exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu "$@"
    ''}")
    , ("M4-<Insert>", spawn "${pkgs.writeDash "paste" ''
      ${pkgs.coreutils}/bin/sleep 0.1
      ${pkgs.xclip}/bin/xclip -o | ${pkgs.xdotool}/bin/xdotool type -f -
    ''}")

    , ("M4-<F5>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -set 1")
    , ("M4-<F6>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -set 10")
    , ("M4-<F7>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -set 33")
    , ("M4-<F8>", spawn "${pkgs.xorg.xbacklight}/bin/xbacklight -set 100")

    , ("<Pause>", spawn "${pkgs.xcalib}/bin/xcalib -invert -alter")

    , ("M4-s", spawn "${pkgs.knav}/bin/knav")

    --, ("M4-w", screenWorkspace 0 >>= (windows . W.greedyView))
    --, ("M4-e", screenWorkspace 1 >>= (windows . W.greedyView))
    --, ("M4-r", screenWorkspace 2 >>= (windows . W.greedyView))
    ]

forkFile :: FilePath -> [String] -> Maybe [(String, String)] -> X ()
forkFile path args env =
    xfork (executeFile path False args env) >> return ()

myXPConfig :: XPConfig
myXPConfig = def
    { font = myFont
    }

autoXPConfig :: XPConfig
autoXPConfig = myXPConfig
    { autoComplete = Just 5000
    }

infixAutoXPConfig :: XPConfig
infixAutoXPConfig = autoXPConfig
    { searchPredicate = isInfixOf
    }

pagerConfig :: PagerConfig
pagerConfig = def
    { pc_font           = myFont
    , pc_cellwidth      = 64
    , pc_matchmethod    = MatchPrefix
    , pc_windowColors   = windowColors
    }
    where
    windowColors _ _ _ True _ = ("#ef4242","#ff2323")
    windowColors wsf m c u wf = do
        let y = defaultWindowColors wsf m c u wf
        if m == False && wf == True
            then ("#402020", snd y)
            else y

gridConfig :: GSConfig WorkspaceId
gridConfig = def
    { gs_cellwidth = 100
    , gs_cellheight = 30
    , gs_cellpadding = 2
    , gs_navigate = navNSearch
    , gs_font = myFont
    }

allWorkspaceNames :: W.StackSet i l a sid sd -> X [i]
allWorkspaceNames ws =
    return $ map W.tag (W.hidden ws) ++ [W.tag $ W.workspace $ W.current ws]
    '';
  };
}
