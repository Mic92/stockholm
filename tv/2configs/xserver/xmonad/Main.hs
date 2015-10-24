{-# LANGUAGE DeriveDataTypeable #-} -- for XS


module Main where

import XMonad
import XMonad.Prompt (defaultXPConfig)
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
import Util.Pager
import Util.Rhombus
import Util.Debunk


--data MyState = MyState deriving Typeable

myTerm :: String
myTerm = "urxvtc"

myRootTerm :: String
myRootTerm = "XENVIRONMENT=\"$HOME/.Xdefaults/root-urxvt\" urxvtc -e su -"

-- TODO execRootTerm = exec (shlex "urxvtc -e su -")
--    [ ("XENVIRONMENT", HOME ++ "/.Xdefaults/root-urxvt") ]


myFont :: String
myFont = "-schumacher-*-*-*-*-*-*-*-*-*-*-*-iso10646-*"

main :: IO ()
main = do
    -- TODO exec (shlex "xrdb -merge" ++ [HOME ++ "/.Xresources"])
    -- TODO exec (shlex "xsetroot -solid '#1c1c1c'")
    --spawn "xrdb -merge \"$HOME/.Xresources\""
    --spawn "xsetroot -solid '#1c1c1c'"
    xmonad
        -- $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "magenta", "-fg", "magenta", "-h", "2"], duration = 500000 }
        --                   urgencyConfig { remindWhen = Every 1 }
        -- $ withUrgencyHook borderUrgencyHook "magenta"
        -- $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "magenta" } urgencyConfig { suppressWhen = Never }
        $ withUrgencyHook (SpawnUrgencyHook "echo emit Urgency ")
        $ defaultConfig
            { terminal          = myTerm
            , modMask           = mod4Mask
            , keys              = myKeys
            , workspaces        =
                [ "Dashboard" -- we start here
                , "23"
                , "cr"
                , "ff"
                , "hack"
                , "im"
                , "mail"
                , "zalora", "zjournal", "zskype"
                ]
            , layoutHook        = smartBorders $ myLayout
            -- , handleEventHook   = myHandleEventHooks <+> handleTimerEvent
            --, handleEventHook   = handleTimerEvent
            , manageHook        = placeHook (smart (1,0)) <+> floatNextHook
            , startupHook       = spawn "echo emit XMonadStartup"
            , normalBorderColor  = "#1c1c1c"
            , focusedBorderColor = "#f000b0"
            }
  where
    myLayout =
        (onWorkspace "im" $ reflectVert $ Mirror $ Tall 1 (3/100) (12/13))
        (FixedColumn 1 20 80 10 ||| Full)


spawnTermAt :: String -> X ()
--spawnTermAt _ = floatNext True >> spawn myTerm
--spawnTermAt "ff" = floatNext True >> spawn myTerm
spawnTermAt _    = spawn myTerm



--jojo w = withDisplay $ \d -> liftIO $ do
--    wa <- getWindowAttributes d w
--    printToErrors (wa_width wa, wa_height wa, wa_x wa, wa_y wa)

    --sh <- getWMNormalHints d w
    --bw <- fmap (fi . wa_border_width) $ getWindowAttributes d w
    --return $ applySizeHints bw sh


--data WindowDetails = WindowDetails
--    { wd_name :: Maybe String
--    , wd_rect :: Rectangle
--    } deriving (Show)

-- urxvtc
--  -title sets {,_NET_}WM_NAME but not WM_CLASS and {,_NET_}WM_ICON_NAME       res: title
--  -name sets all                                                              res: 
--mySpawn cmd = do
--    p <- xfork $ executeFile "/run/current-system/sw/bin/urxvtc" False [] Nothing
--    liftIO $ printToErrors $ (cmd, p)


myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf = Map.fromList $
    [ ((_4C , xK_Delete ), spawn "make -C $HOME/.xmonad reload")
    , ((_4  , xK_Escape ), spawn "/var/setuid-wrappers/slock")
    , ((_4S , xK_c      ), kill)

    , ((_4  , xK_x      ), chooseAction spawnTermAt)
    , ((_4C , xK_x      ), spawn myRootTerm)
    --, ((_4M , xK_x      ), spawn "xterm")
    --, ((_4M , xK_x      ), mySpawn "xterm")

    --, ((_4  , xK_F1     ), withFocused jojo)
    --, ((_4  , xK_F1     ), printAllGeometries)

    , ((0   , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.view) )
    , ((_S  , xK_Menu   ), gets windowset >>= allWorkspaceNames >>= pager pagerConfig (windows . W.shift) )
    , ((_C  , xK_Menu   ), toggleWS)
    , ((_4  , xK_Menu   ), rhombus horseConfig (liftIO . printToErrors) ["Correct", "Horse", "Battery", "Staple", "Stuhl", "Tisch"] )
    
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

    , ((_4  , xK_a      ), addWorkspacePrompt defaultXPConfig)
    , ((_4  , xK_r      ), renameWorkspace defaultXPConfig)
    , ((_4  , xK_Delete ), removeEmptyWorkspace)

    , ((_4  , xK_Return ), toggleWS)
    --,  (0   , xK_Menu   ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.view)
    --,  (_4  , xK_v      ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.view)
    --,  (_4S , xK_v      ) & \k -> (k, gridselectWorkspace wsGSConfig { gs_navigate = makeGSNav k } W.shift)
    --,  (_4  , xK_b      ) & \k -> (k, goToSelected        wGSConfig  { gs_navigate = makeGSNav k })
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


pagerConfig :: PagerConfig
pagerConfig = defaultPagerConfig
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
        let def = defaultWindowColors wsf m c u wf
        if m == False && wf == True
            then ("#402020", snd def)
            else def

horseConfig :: RhombusConfig
horseConfig = defaultRhombusConfig
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
wGSConfig = defaultGSConfig
    { gs_cellheight = 20
    , gs_cellwidth = 192
    , gs_cellpadding = 5
    , gs_font = myFont
    , gs_navigate = navNSearch
    }

-- wsGSConfig = defaultGSConfig
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

-- vim:set fdm=marker:
