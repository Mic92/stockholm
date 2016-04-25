{-# LANGUAGE DeriveDataTypeable #-} -- for XS
{-# LANGUAGE FlexibleContexts #-} -- for xmonad'
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Control.Exception
import Text.Read (readEither)
import XMonad
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, withArgs, getEnv, getEnvironment)
import System.Posix.Process (executeFile)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace
                                        , removeEmptyWorkspace)
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS (toggleWS)
--import XMonad.Actions.CopyWindow ( copy )
import XMonad.Layout.NoBorders ( smartBorders )
import qualified XMonad.StackSet as W
import Data.Map (Map)
import qualified Data.Map as Map
-- TODO import XMonad.Layout.WorkspaceDir
import XMonad.Hooks.UrgencyHook (SpawnUrgencyHook(..), withUrgencyHook)
-- import XMonad.Layout.Tabbed
--import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.FixedColumn (FixedColumn(..))
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.FloatNext (floatNextHook)
import XMonad.Actions.PerWorkspaceKeys (chooseAction)
import XMonad.Layout.PerWorkspace (onWorkspace)
--import XMonad.Layout.BinarySpacePartition
import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Prompt (autoComplete, defaultXPConfig, XPConfig, mkXPrompt)
import XMonad.Hooks.UrgencyHook (focusUrgent, withUrgencyHook, urgencyBorderColor, BorderUrgencyHook(BorderUrgencyHook))
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace, renameWorkspace, withWorkspace)
import XMonad.Hooks.FloatNext (floatNext, floatNextHook)
import XMonad.Prompt.Workspace
import XMonad.Actions.CopyWindow (copy, kill1)
import qualified Data.Map as M
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(ToggleStruts))

--import XMonad.Actions.Submap
import XMonad.Stockholm.Pager
import XMonad.Stockholm.Rhombus
import XMonad.Stockholm.Shutdown


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
        -- $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "magenta", "-fg", "magenta", "-h", "2"], duration = 500000 }
        --                   urgencyConfig { remindWhen = Every 1 }
        -- $ withUrgencyHook borderUrgencyHook "magenta"
        -- $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "magenta" } urgencyConfig { suppressWhen = Never }
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = myTerm
            , modMask           = mod4Mask
            , workspaces        = workspaces0
            , layoutHook = smartBorders $ myLayoutHook
            -- , handleEventHook   = myHandleEventHooks <+> handleTimerEvent
            --, handleEventHook   = handleTimerEvent
            , manageHook        = placeHook (smart (1,0)) <+> floatNextHook
            , startupHook       = spawn "echo emit XMonadStartup"
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            , handleEventHook = handleShutdownEvent
            } `additionalKeysP` myKeyMap

myLayoutHook = defLayout
  where
    defLayout = (avoidStruts $ Tall 1 (3/100) (1/2) ||| Full ||| Mirror (Tall 1 (3/100) (1/2))) ||| FixedColumn 2 80 80 1


xmonad' :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad' conf = do
    path <- getEnv "XMONAD_STATE"
    try (readFile path) >>= \case
        Right content -> do
            hPutStrLn stderr ("resuming from " ++ path)
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


myKeyMap =
    [ ("M4-<F11>", spawn "i3lock -i /tmp/wallpaper.png -f")
    , ("M4-p", spawn "passmenu --type")
    --, ("M4-r", spawn "exe=$(yeganesh -x) && eval \"exec $exe\"")
    , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +4%")
    , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -4%")
    , ("<XF86AudioMute>", spawn "pactl -- set-sink-mute 0 toggle")
    , ("<XF86AudioMicMute>", spawn "pactl -- set-source-mute 1 toggle")
    , ("<XF86Launch1>", gridselectWorkspace myWSConfig W.view)

    , ("M4-a", focusUrgent)
    , ("M4-S-r", renameWorkspace    defaultXPConfig)
    , ("M4-S-a", addWorkspacePrompt defaultXPConfig)
    , ("M4-S-<Backspace>", removeEmptyWorkspace)
    , ("M4-S-c", kill1)
    , ("M4-<Esc>", toggleWS)
    , ("M4-S-<Enter>", spawn myTerm)
    , ("M4-x", floatNext True >> spawn myTerm)
    , ("M4-f", floatNext True)
    , ("M4-b", sendMessage ToggleStruts)

    , ("M4-v", withWorkspace myXPConfig (windows . W.view))
    , ("M4-S-v", withWorkspace myXPConfig (windows . W.shift))
    , ("M4-C-v", withWorkspace myXPConfig (windows . copy))

    -- , (_4 , xK_q      ) & \k -> (k, goToSelected myCNConfig { gs_navigate = makeGSNav k }                   )
    -- , (_4S, xK_q      ) & \k -> (k, bringSelected myCNConfig { gs_navigate = makeGSNav k }                  )
    -- , (_4C, xK_q      ) & \k -> (k, withSelectedWindow ( \a -> get >>= \s -> put s { windowset = copyWindow a (W.tag $ W.workspace $ W.current $ windowset s) (windowset s) } ) myCNConfig { gs_navigate = makeGSNav k } )

    --, ("M4-<F1>", perWorkspaceAction workspaceConfigs)
    , ("M4-S-q", return ())
    ]

myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellpadding = 2
    , gs_navigate = navNSearch
    , gs_font = myFont
    }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { autoComplete = Just 5000
    }

myWSConfig = myGSConfig
    { gs_cellwidth = 50
    }

pagerConfig :: PagerConfig
pagerConfig = def
    { pc_font           = myFont
    , pc_cellwidth      = 64
    --, pc_cellheight     = 36 -- TODO automatically keep screen aspect
    --, pc_borderwidth    = 1
    --, pc_matchcolor     = "#f0b000"
    , pc_matchmethod    = MatchPrefix
    --, pc_colors         = pagerWorkspaceColors
    , pc_windowColors   = windowColors
    }
    where
    windowColors _ _ _ True _ = ("#ef4242","#ff2323")
    windowColors wsf m c u wf = do
        let y = defaultWindowColors wsf m c u wf
        if m == False && wf == True
            then ("#402020", snd y)
            else y

wGSConfig :: GSConfig Window
wGSConfig = def
    { gs_cellheight = 20
    , gs_cellwidth = 192
    , gs_cellpadding = 5
    , gs_font = myFont
    , gs_navigate = navNSearch
    }


(&) :: a -> (a -> c) -> c
(&) = flip ($)

allWorkspaceNames :: W.StackSet i l a sid sd -> X [i]
allWorkspaceNames ws =
    return $ map W.tag (W.hidden ws) ++ [W.tag $ W.workspace $ W.current ws]
