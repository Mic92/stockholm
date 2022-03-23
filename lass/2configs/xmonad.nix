{ config, lib, pkgs, ... }:

{
  services.xserver.windowManager.xmonad = {
    enable = true;
    extraPackages = hs: [
      hs.extra
      hs.xmonad-stockholm
    ];
    config = /* haskell */ ''
{-# LANGUAGE LambdaCase #-}


module Main where
import XMonad

import qualified XMonad.StackSet as W
import Control.Monad.Extra (whenJustM)
import Data.List (isInfixOf)
import Data.Monoid (Endo)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import Data.Ratio

import XMonad.Actions.CopyWindow (copy, copyToAll, kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace, removeEmptyWorkspace)
import XMonad.Actions.DynamicWorkspaces (withWorkspace)
import XMonad.Actions.GridSelect (GSConfig(..), gridselectWorkspace, navNSearch)
import XMonad.Actions.Minimize (minimizeWindow, maximizeWindow, withLastMinimized)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FloatNext (floatNext)
import XMonad.Hooks.FloatNext (floatNextHook)
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat, (-?>))
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, UrgencyHook(..))
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusUp)
import XMonad.Layout.FixedColumn (FixedColumn(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MouseResizableTile (mouseResizableTile)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.ManageHook (composeAll)
import XMonad.Prompt (autoComplete, font, searchPredicate, XPConfig)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBringCopy)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Ungrab (unGrab)

import XMonad.Stockholm.Shutdown (newShutdownEventHandler, shutdown)
import XMonad.Stockholm.Pager (defaultWindowColors, pager, MatchMethod(MatchPrefix), PagerConfig(..))

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "${pkgs.libnotify}/bin/notify-send" [show name, "workspace " ++ idx]

myTerm :: FilePath
-- myTerm = "${pkgs.rxvt_unicode-with-plugins}/bin/urxvtc -e /run/current-system/sw/bin/xonsh"
-- myTerm = "${pkgs.rxvt_unicode-with-plugins}/bin/urxvtc"
myTerm = "/run/current-system/sw/bin/alacritty"

myFont :: String
myFont = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1"

main :: IO ()
main = getArgs >>= \case
    [] -> main'
    ["--shutdown"] -> shutdown
    args -> hPutStrLn stderr ("bad arguments: " <> show args) >> exitFailure

main' :: IO ()
main' = do
    handleShutdownEvent <- newShutdownEventHandler
    launch $ ewmh
        $ withUrgencyHook LibNotifyUrgencyHook
        $ def
            { terminal           = myTerm
            , modMask            = mod4Mask
            , layoutHook         = smartBorders $ myLayoutHook
            , manageHook         = floatHooks
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
    defLayout = minimize . boringWindows $ ((avoidStruts $ Mirror (Tall 1 (3/100) (1/2))) ||| Full ||| FixedColumn 2 80 80 1 ||| Tall 1 (3/100) (1/2) ||| simplestFloat ||| mouseResizableTile ||| Grid)

floatHooks = composeAll
   [ className =? "Pinentry" --> doCenterFloat
   , title =? "fzfmenu" --> doCenterFloat
   , title =? "glxgears" --> doCenterFloat
   , resource =? "Dialog" --> doFloat
   , title =? "Upload to Imgur" -->
       doRectFloat (W.RationalRect 0 0 (1 % 8) (1 % 8))
   , placeHook (smart (1,0))
   , floatNextHook
   ]

myKeyMap :: [([Char], X ())]
myKeyMap =
    [ ("M4-C-p", forkFile "${pkgs.scrot}/bin/scrot" [ "~/public_html/scrot.png" ] Nothing )
    , ("M4-p", forkFile "${pkgs.pass}/bin/passmenu" [ "--type" ] Nothing)
    , ("M4-S-p", forkFile "${pkgs.otpmenu}/bin/otpmenu" [] Nothing)
    , ("M4-o", forkFile "${pkgs.brain}/bin/brainmenu --type" [] Nothing)
    , ("M4-z", forkFile "${pkgs.emot-menu}/bin/emoticons" [] Nothing)

    , ("M4-S-q", restart "xmonad" True)

    , ("<XF86AudioMute>", spawn "${pkgs.pulseaudio.out}/bin/pactl -- set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioRaiseVolume>", spawn "${pkgs.pulseaudio.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ +4%")
    , ("<XF86AudioLowerVolume>", spawn "${pkgs.pulseaudio.out}/bin/pactl -- set-sink-volume @DEFAULT_SINK@ -4%")
    , ("<XF86MonBrightnessDown>", spawn "${pkgs.acpilight}/bin/xbacklight -time 0 -dec 1")
    , ("<XF86MonBrightnessUp>",   spawn "${pkgs.acpilight}/bin/xbacklight -time 0 -inc 1")
    , ("M4-C-k", spawn "${pkgs.xorg.xkill}/bin/xkill")

    , ("M4-<Tab>", focusDown)
    , ("M4-S-<Tab>", focusUp)
    , ("M4-j", focusDown)
    , ("M4-k", focusUp)

    , ("M4-a", focusUrgent)
    , ("M4-S-r", renameWorkspace    myXPConfig)
    , ("M4-S-a", addWorkspacePrompt myXPConfig)
    , ("M4-S-<Backspace>", removeEmptyWorkspace)
    , ("M4-S-c", kill1)
    , ("M4-<Esc>", toggleWS)
    , ("M4-S-<Enter>", spawn myTerm)
    , ("M4-x", floatNext True >> spawn myTerm)
    , ("M4-c", spawn "/run/current-system/sw/bin/emacsclient -c")
    -- , ("M4-c", unGrab)
    , ("M4-f", floatNext True)
    , ("M4-b", spawn "/run/current-system/sw/bin/klem")

    , ("M4-v", gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.greedyView) )
    , ("M4-S-v", gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ("M4-C-v", withWorkspace autoXPConfig (windows . copy))

    , ("M4-m", withFocused minimizeWindow)
    , ("M4-S-m", withLastMinimized maximizeWindow)

    , ("M4-q", windowPromptGoto infixAutoXPConfig)
    , ("M4-C-q", windowPromptBringCopy infixAutoXPConfig)

    , ("M4-S-q", return ())

    , ("M4-d", floatNext True >> spawn "${pkgs.copyq}/bin/copyq show")

    , ("M4-<F2>", windows copyToAll)

    , ("M4-<F4>", spawn "${pkgs.nm-dmenu}/bin/nm-dmenu")
    , ("M4-<Insert>", spawn "${pkgs.writeDash "paste" ''
      ${pkgs.coreutils}/bin/sleep 0.1
      ${pkgs.xclip}/bin/xclip -o | ${pkgs.xdotool}/bin/xdotool type -f -
    ''}")

    , ("M4-<F5>", spawn "${pkgs.acpilight}/bin/xbacklight -set 1")
    , ("M4-<F6>", spawn "${pkgs.acpilight}/bin/xbacklight -set 10")
    , ("M4-<F7>", spawn "${pkgs.acpilight}/bin/xbacklight -set 33")
    , ("M4-<F8>", spawn "${pkgs.acpilight}/bin/xbacklight -set 100")

    , ("M4-<F9>", spawn "${pkgs.redshift}/bin/redshift -O 4000 -g 0.9:0.8:0.8")
    , ("M4-<F10>", spawn "${pkgs.redshift}/bin/redshift -x")

    , ("M4-<F11>", spawn "${config.lass.screenlock.command}")
    , ("M4-<F12>", spawn "${pkgs.systemd}/bin/systemctl suspend -i")

    , ("M4-u", spawn "${pkgs.xcalib}/bin/xcalib -invert -alter")

    , ("M4-s", spawn "${pkgs.knav}/bin/knav")
    , ("M4-i", spawn "/run/current-system/sw/bin/screenshot")

    --, ("M4-w", screenWorkspace 0 >>= (windows . W.greedyView))
    --, ("M4-e", screenWorkspace 1 >>= (windows . W.greedyView))
    --, ("M4-r", screenWorkspace 2 >>= (windows . W.greedyView))
    ]

forkFile :: FilePath -> [String] -> Maybe [(String, String)] -> X ()
forkFile path args env =
    xfork (executeFile path True args env) >> return ()

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
    return $ map W.tag (W.hidden ws ++ (map W.workspace $ W.visible ws)) ++ [W.tag $ W.workspace $ W.current ws]
    '';
  };
}
