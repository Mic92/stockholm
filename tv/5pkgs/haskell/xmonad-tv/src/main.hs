{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Exit (exitFailure)

import Control.Exception
import Control.Monad.Extra (whenJustM)
import Graphics.X11.ExtraTypes.XF86
import Text.Read (readEither)
import XMonad
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, getEnv, getEnvironment, lookupEnv)
import System.Posix.Process (executeFile)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace
                                        , removeEmptyWorkspace)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.ResizableTile (MirrorResize(MirrorExpand,MirrorShrink))
import qualified XMonad.StackSet as W
import Data.Map (Map)
import qualified Data.Map as Map
import XMonad.Hooks.UrgencyHook (SpawnUrgencyHook(..), withUrgencyHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat,doRectFloat)
import Data.Ratio
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Actions.PerWorkspaceKeys (chooseAction)

import XMonad.Stockholm.Pager
import XMonad.Stockholm.Shutdown
import qualified Paths


myFont :: String
myFont = "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"


main :: IO ()
main = getArgs >>= \case
    [] -> mainNoArgs
    ["--shutdown"] -> shutdown
    args -> hPutStrLn stderr ("bad arguments: " <> show args) >> exitFailure


mainNoArgs :: IO ()
mainNoArgs = do
    let width = 1366
    workspaces0 <- getWorkspaces0
    handleShutdownEvent <- newShutdownEventHandler
    launch
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = Paths.urxvtc
            , modMask           = mod4Mask
            , keys              = myKeys
            , workspaces        = workspaces0
            , layoutHook =
                smartBorders $
                  ResizableTall
                    1
                    (10 * 6 / width)
                    ((80 * 6 + 2 * (1+1+1))/width) []
                  |||
                  Full
            , manageHook =
                composeAll
                  [ appName =? "fzmenu-urxvt" --> doCenterFloat
                  , appName =? "pinentry" --> doCenterFloat
                  , title =? "Upload to Imgur" -->
                      doRectFloat (W.RationalRect 0 0 (1 % 8) (1 % 8))
                  , placeHook (smart (1,0))
                  ]
            , startupHook =
                whenJustM (io (lookupEnv "XMONAD_STARTUP_HOOK"))
                          (\path -> forkFile path [] Nothing)
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            , handleEventHook = handleShutdownEvent
            }


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


forkFile :: FilePath -> [String] -> Maybe [(String, String)] -> X ()
forkFile path args env =
    xfork (executeFile path False args env) >> return ()


spawnRootTerm :: X ()
spawnRootTerm =
    forkFile
        Paths.urxvtc
        ["-name", "root-urxvt", "-e", Paths.su, "-"]
        Nothing


spawnTermAt :: String -> X ()
spawnTermAt ws = do
    env <- io getEnvironment
    let env' = ("XMONAD_SPAWN_WORKSPACE", ws) : env
    forkFile Paths.urxvtc [] (Just env')


myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf = Map.fromList $
    [ ((_4  , xK_Escape ), forkFile Paths.slock [] Nothing)
    , ((_4S , xK_c      ), kill)

    , ((_4  , xK_o      ), forkFile Paths.otpmenu [] Nothing)
    , ((_4  , xK_p      ), forkFile Paths.passmenu [] Nothing)

    , ((_4  , xK_x      ), chooseAction spawnTermAt)
    , ((_4C , xK_x      ), spawnRootTerm)

    , ((0   , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.view) )
    , ((_S  , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ((_C  , xK_Menu   ), toggleWS)

    , ((_4  , xK_space  ), sendMessage NextLayout)
    , ((_4M , xK_space  ), resetLayout)

    , ((_4  , xK_m      ), windows W.focusMaster)
    , ((_4  , xK_j      ), windows W.focusDown)
    , ((_4  , xK_k      ), windows W.focusUp)

    , ((_4S , xK_m      ), windows W.swapMaster)
    , ((_4S , xK_j      ), windows W.swapDown)
    , ((_4S , xK_k      ), windows W.swapUp)

    , ((_4M , xK_h      ), sendMessage Shrink)
    , ((_4M , xK_l      ), sendMessage Expand)

    , ((_4M , xK_j      ), sendMessage MirrorShrink)
    , ((_4M , xK_k      ), sendMessage MirrorExpand)

    , ((_4  , xK_t      ), withFocused $ windows . W.sink)

    , ((_4  , xK_comma  ), sendMessage $ IncMasterN 1)
    , ((_4  , xK_period ), sendMessage $ IncMasterN (-1))

    , ((_4  , xK_a      ), addWorkspacePrompt def)
    , ((_4  , xK_r      ), renameWorkspace def)
    , ((_4  , xK_Delete ), removeEmptyWorkspace)

    , ((_4  , xK_Return ), toggleWS)

    , ((0, xF86XK_AudioLowerVolume), audioLowerVolume)
    , ((0, xF86XK_AudioRaiseVolume), audioRaiseVolume)
    , ((0, xF86XK_AudioMute), audioMute)
    , ((_4, xF86XK_AudioMute), pavucontrol [])

    , ((_4, xK_Prior), forkFile Paths.xcalib ["-invert", "-alter"] Nothing)

    , ((0, xK_Print), forkFile Paths.flameshot [] Nothing)
    ]
    where
    _4 = mod4Mask
    _C = controlMask
    _S = shiftMask
    _M = mod1Mask
    _4C = _4 .|. _C
    _4S = _4 .|. _S
    _4M = _4 .|. _M
    _4CM = _4 .|. _C .|. _M
    _4SM = _4 .|. _S .|. _M

    pactl args = forkFile Paths.pactl args Nothing
    pavucontrol args = forkFile Paths.pavucontrol args Nothing

    audioLowerVolume = pactl ["--", "set-sink-volume", "@DEFAULT_SINK@", "-5%"]
    audioRaiseVolume = pactl ["--", "set-sink-volume", "@DEFAULT_SINK@", "+5%"]
    audioMute = pactl ["--", "set-sink-mute", "@DEFAULT_SINK@", "toggle"]

    resetLayout = setLayout $ XMonad.layoutHook conf


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


allWorkspaceNames :: W.StackSet i l a sid sd -> X [i]
allWorkspaceNames = return . map W.tag . W.workspaces
