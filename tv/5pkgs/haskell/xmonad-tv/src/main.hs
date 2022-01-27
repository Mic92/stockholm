{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import System.Exit (exitFailure)

import Control.Exception
import Control.Monad.Extra (ifM, whenJustM)
import qualified Data.List
import Graphics.X11.ExtraTypes.XF86
import Text.Read (readEither)
import XMonad
import XMonad.Extra (isFloatingX)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, getEnv, getEnvironment, lookupEnv)
import System.Posix.Process (executeFile)
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt, renameWorkspace
                                        , removeEmptyWorkspace)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.ResizableTile (MirrorResize(MirrorExpand,MirrorShrink))
import qualified XMonad.Prompt
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

import THEnv.JSON (getCompileEnvJSONExp)


myFont :: String
myFont = "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"

myScreenWidth :: Dimension
myScreenWidth =
  $(getCompileEnvJSONExp (id @Dimension) "XMONAD_BUILD_SCREEN_WIDTH")

myTermFontWidth :: Dimension
myTermFontWidth =
  $(getCompileEnvJSONExp (id @Dimension) "XMONAD_BUILD_TERM_FONT_WIDTH")

myTermPadding :: Dimension
myTermPadding = 2


main :: IO ()
main = getArgs >>= \case
    [] -> mainNoArgs
    ["--shutdown"] -> shutdown
    args -> hPutStrLn stderr ("bad arguments: " <> show args) >> exitFailure


(=??) :: Query a -> (a -> Bool) -> Query Bool
(=??) x p = fmap p x


mainNoArgs :: IO ()
mainNoArgs = do
    workspaces0 <- getWorkspaces0
    handleShutdownEvent <- newShutdownEventHandler
    launch
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = {-pkg:rxvt_unicode-}"urxvtc"
            , modMask           = mod4Mask
            , keys              = myKeys
            , workspaces        = workspaces0
            , layoutHook =
                smartBorders $
                  ResizableTall
                    1
                    (fromIntegral (10 * myTermFontWidth) / fromIntegral myScreenWidth)
                    (fromIntegral (80 * myTermFontWidth + 2 * (myTermPadding + borderWidth def)) / fromIntegral myScreenWidth)
                    []
                  |||
                  Full
            , manageHook =
                composeAll
                  [ appName =? "fzmenu-urxvt" --> doCenterFloat
                  , appName =?? Data.List.isPrefixOf "pinentry" --> doCenterFloat
                  , appName =?? Data.List.isInfixOf "Float" --> doCenterFloat
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
    xfork (executeFile path True args env) >> return ()


spawnRootTerm :: X ()
spawnRootTerm =
    forkFile
        {-pkg:rxvt_unicode-}"urxvtc"
        ["-name", "root-urxvt", "-e", "/run/wrappers/bin/su", "-"]
        Nothing


spawnTermAt :: String -> X ()
spawnTermAt ws = do
    env <- io getEnvironment
    let env' = ("XMONAD_SPAWN_WORKSPACE", ws) : env
    forkFile {-pkg:rxvt_unicode-}"urxvtc" [] (Just env')


myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf = Map.fromList $
    [ ((_4  , xK_Escape ), forkFile {-pkg-}"slock" [] Nothing)
    , ((_4S , xK_c      ), kill)

    , ((_4  , xK_o      ), forkFile {-pkg:fzmenu-}"otpmenu" [] Nothing)
    , ((_4  , xK_p      ), forkFile {-pkg:fzmenu-}"passmenu" [] Nothing)

    , ((_4  , xK_x      ), chooseAction spawnTermAt)
    , ((_4C , xK_x      ), spawnRootTerm)

    , ((0   , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.view) )
    , ((_S  , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ((_C  , xK_Menu   ), toggleWS)

    , ((_4  , xK_space  ), withFocused $ \w -> ifM (isFloatingX w) xdeny $ sendMessage NextLayout)
    , ((_4M , xK_space  ), withFocused $ \w -> ifM (isFloatingX w) xdeny $ resetLayout)

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

    , ((_4  , xK_a      ), addWorkspacePrompt promptXPConfig)
    , ((_4  , xK_r      ), renameWorkspace promptXPConfig)
    , ((_4  , xK_Delete ), removeEmptyWorkspace)

    , ((_4  , xK_Return ), toggleWS)

    , ((0, xF86XK_AudioLowerVolume), audioLowerVolume)
    , ((0, xF86XK_AudioRaiseVolume), audioRaiseVolume)
    , ((0, xF86XK_AudioMute), audioMute)
    , ((_4, xF86XK_AudioMute), pavucontrol [])

    , ((_4, xK_Prior), forkFile {-pkg-}"xcalib" ["-invert", "-alter"] Nothing)

    , ((0, xK_Print), forkFile {-pkg-}"flameshot" [] Nothing)

    , ((_C, xF86XK_Forward), forkFile {-pkg:xdpytools-}"xdpychvt" ["next"] Nothing)
    , ((_C, xF86XK_Back), forkFile {-pkg:xdpytools-}"xdpychvt" ["prev"] Nothing)
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

    amixer args = forkFile {-pkg:alsaUtils-}"amixer" args Nothing
    pavucontrol args = forkFile {-pkg-}"pavucontrol" args Nothing

    audioLowerVolume = amixer ["-q", "sset", "Master", "5%-"]
    audioRaiseVolume = amixer ["-q", "sset", "Master", "5%+"]
    audioMute = amixer ["-q", "sset", "Master", "toggle"]

    resetLayout = setLayout $ XMonad.layoutHook conf

    promptXPConfig =
        def { XMonad.Prompt.font = myFont }


xdeny :: X ()
xdeny =
    forkFile
        {-pkg-}"xterm"
        [ "-fn", myFont
        , "-geometry", "300x100"
        , "-name", "AlertFloat"
        , "-bg", "#E4002B"
        , "-e", "sleep", "0.05"
        ]
        Nothing


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
