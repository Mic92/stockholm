{ pkgs, ... }:
pkgs.writeHaskell "xmonad-tv" {
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

import Control.Exception
import Graphics.X11.ExtraTypes.XF86
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

--import XMonad.Actions.Submap
import XMonad.Stockholm.Pager
import XMonad.Stockholm.Rhombus
import XMonad.Stockholm.Shutdown


amixerPath :: FilePath
amixerPath = "${pkgs.alsaUtils}/bin/amixer"

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
    workspaces0 <- getWorkspaces0
    xmonad'
        -- $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "magenta", "-fg", "magenta", "-h", "2"], duration = 500000 }
        --                   urgencyConfig { remindWhen = Every 1 }
        -- $ withUrgencyHook borderUrgencyHook "magenta"
        -- $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "magenta" } urgencyConfig { suppressWhen = Never }
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ def
            { terminal          = urxvtcPath
            , modMask           = mod4Mask
            , keys              = myKeys
            , workspaces        = workspaces0
            , layoutHook        = smartBorders $ myLayout
            -- , handleEventHook   = myHandleEventHooks <+> handleTimerEvent
            --, handleEventHook   = handleTimerEvent
            , manageHook        = placeHook (smart (1,0)) <+> floatNextHook
            , startupHook = do
                path <- liftIO (getEnv "XMONAD_STARTUP_HOOK")
                forkFile path [] Nothing
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            , handleEventHook = handleShutdownEvent
            }
  where
    myLayout =
        (onWorkspace "im" $ reflectVert $ Mirror $ Tall 1 (3/100) (12/13))
        (FixedColumn 1 20 80 10 ||| Full)


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


forkFile :: FilePath -> [String] -> Maybe [(String, String)] -> X ()
forkFile path args env =
    xfork (executeFile path False args env) >> return ()

spawnRootTerm :: X ()
spawnRootTerm =
    forkFile
        urxvtcPath
        ["-name", "root-urxvt", "-e", "/var/setuid-wrappers/su", "-"]
        Nothing

spawnTermAt :: String -> X ()
spawnTermAt ws = do
    env <- liftIO getEnvironment
    let env' = ("XMONAD_SPAWN_WORKSPACE", ws) : env
    forkFile urxvtcPath [] (Just env')

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf = Map.fromList $
    [ ((_4  , xK_Escape ), forkFile "/var/setuid-wrappers/slock" [] Nothing)
    , ((_4S , xK_c      ), kill)

    , ((_4  , xK_x      ), chooseAction spawnTermAt)
    , ((_4C , xK_x      ), spawnRootTerm)

    --, ((_4  , xK_F1     ), withFocused jojo)
    --, ((_4  , xK_F1     ), printAllGeometries)

    , ((0   , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.view) )
    , ((_S  , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ((_C  , xK_Menu   ), toggleWS)
    , ((_4  , xK_Menu   ), rhombus horseConfig (liftIO . hPutStrLn stderr) ["Correct", "Horse", "Battery", "Staple", "Stuhl", "Tisch"] )
    
    -- %! Rotate through the available layout algorithms
    , ((_4  , xK_space  ), sendMessage NextLayout)
    , ((_4S , xK_space  ), setLayout $ XMonad.layoutHook conf) -- reset layout

    ---- BinarySpacePartition
    --, ((_4  , xK_l), sendMessage $ ExpandTowards R)
    --, ((_4  , xK_h), sendMessage $ ExpandTowards L)
    --, ((_4  , xK_j), sendMessage $ ExpandTowards D)
    --, ((_4  , xK_k), sendMessage $ ExpandTowards U)
    --, ((_4S , xK_l), sendMessage $ ShrinkFrom R)
    --, ((_4S , xK_h), sendMessage $ ShrinkFrom L)
    --, ((_4S , xK_j), sendMessage $ ShrinkFrom D)
    --, ((_4S , xK_k), sendMessage $ ShrinkFrom U)
    --, ((_4  , xK_n), sendMessage Rotate)
    --, ((_4S , xK_n), sendMessage Swap)

    ---- mouseResizableTile
    --, ((_4    , xK_u), sendMessage ShrinkSlave)
    --, ((_4    , xK_i), sendMessage ExpandSlave)

    -- move focus up or down the window stack
    --, ((_4  , xK_m      ), windows W.focusMaster)
    , ((_4  , xK_j      ), windows W.focusDown)
    , ((_4  , xK_k      ), windows W.focusUp)

    -- modifying the window order
    , ((_4S , xK_m      ), windows W.swapMaster)
    , ((_4S , xK_j      ), windows W.swapDown)
    , ((_4S , xK_k      ), windows W.swapUp)

    -- resizing the master/slave ratio
    , ((_4  , xK_h      ), sendMessage Shrink) -- %! Shrink the master area
    , ((_4  , xK_l      ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((_4  , xK_t      ), withFocused $ windows . W.sink)  -- make tiling

    -- increase or decrease number of windows in the master area
    , ((_4  , xK_comma  ), sendMessage $ IncMasterN 1)
    , ((_4  , xK_period ), sendMessage $ IncMasterN (-1))

    , ((_4  , xK_a      ), addWorkspacePrompt def)
    , ((_4  , xK_r      ), renameWorkspace def)
    , ((_4  , xK_Delete ), removeEmptyWorkspace)

    , ((_4  , xK_Return ), toggleWS)
    --,  (0   , xK_Menu   ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.view)
    --,  (_4  , xK_v      ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.view)
    --,  (_4S , xK_v      ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.shift)
    --,  (_4  , xK_b      ) & \k -> (k, goToSelected        wGSConfig  { gs_navigate = makeGSNav k })
    , ((noModMask, xF86XK_AudioLowerVolume), amixer ["sset", "Master", "5%-"])
    , ((noModMask, xF86XK_AudioRaiseVolume), amixer ["sset", "Master", "5%+"])
    , ((noModMask, xF86XK_AudioMute), amixer ["sset", "Master", "toggle"])
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

    amixer args = forkFile amixerPath args Nothing


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

horseConfig :: RhombusConfig
horseConfig = def
    { rc_font           = myFont
    , rc_cellwidth      = 64
    --, rc_cellheight     = 36 -- TODO automatically keep screen aspect
    --, rc_borderwidth    = 1
    --, rc_matchcolor     = "#f0b000"
    , rc_matchmethod    = MatchPrefix
    --, rc_colors         = pagerWorkspaceColors
    --, rc_paint          = myPaint
    }

wGSConfig :: GSConfig Window
wGSConfig = def
    { gs_cellheight = 20
    , gs_cellwidth = 192
    , gs_cellpadding = 5
    , gs_font = myFont
    , gs_navigate = navNSearch
    }

-- wsGSConfig = def
--     { gs_cellheight = 20
--     , gs_cellwidth = 64
--     , gs_cellpadding = 5
--     , gs_font = myFont
--     , gs_navigate = navNSearch
--     }

-- custom navNSearch
--makeGSNav :: (KeyMask, KeySym) -> TwoD a (Maybe a)
--makeGSNav esc = nav
--    where
--    nav = makeXEventhandler $ shadowWithKeymap keyMap navNSearchDefaultHandler
--    keyMap = Map.fromList
--        [ (esc              , cancel)
--        , ((0,xK_Escape)    , cancel)
--        , ((0,xK_Return)    , select)
--        , ((0,xK_Left)      , move (-1, 0) >> nav)
--        , ((0,xK_Right)     , move ( 1, 0) >> nav)
--        , ((0,xK_Down)      , move ( 0, 1) >> nav)
--        , ((0,xK_Up)        , move ( 0,-1) >> nav)
--        , ((0,xK_BackSpace) , transformSearchString (\s -> if (s == "") then "" else init s) >> nav)
--        ]
--    -- The navigation handler ignores unknown key symbols, therefore we const
--    navNSearchDefaultHandler (_,s,_) = do
--        transformSearchString (++ s)
--        nav


(&) :: a -> (a -> c) -> c
(&) = flip ($)

allWorkspaceNames :: W.StackSet i l a sid sd -> X [i]
allWorkspaceNames ws =
    return $ map W.tag (W.hidden ws) ++ [W.tag $ W.workspace $ W.current ws]
  '';
  };
}
