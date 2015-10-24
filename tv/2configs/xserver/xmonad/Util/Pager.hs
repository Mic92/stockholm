module Util.Pager
    ( defaultPagerConfig
    , defaultWindowColors
    , defaultWorkspaceColors
    , MatchMethod(..)
    , pager
    , PagerConfig(..)
    ) where

import Data.List ( find )
import Data.Maybe ( catMaybes )
import Graphics.X11
import Util.Rhombus
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Font ( fi, stringToPixel )


data PagerConfig = PagerConfig
    { pc_font               :: String
    , pc_cellwidth          :: Dimension
    , pc_margin             :: Dimension
    , pc_matchmethod        :: MatchMethod
    , pc_wrap               :: Bool
    , pc_workspaceColors    :: Bool -> Bool -> Bool -> (String, String, String)
    , pc_windowColors       :: Bool -> Bool -> Bool -> Bool -> Bool -> (String, String)
    }


defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig "xft:Sans-8" 100 0 MatchInfix True defaultWorkspaceColors defaultWindowColors


pager :: PagerConfig -> (String -> X ()) -> [String] -> X ()
pager pc = rhombus defaultRhombusConfig
    { rc_font           = pc_font pc
    , rc_cellwidth      = pc_cellwidth pc
    , rc_margin         = pc_margin pc
    , rc_matchmethod    = pc_matchmethod pc
    , rc_wrap           = pc_wrap pc
    , rc_colors         = pc_workspaceColors pc
    , rc_paint          = pagerPaint pc
    }


defaultWorkspaceColors :: Bool -- workspace has focus
                       -> Bool -- workspace name matches incremental search
                       -> Bool -- workspace is the current one
                       -> (String, String, String) -- workspace border, background color, and foreground color
defaultWorkspaceColors False False False = ("#101010","#050505","#202020")
defaultWorkspaceColors False False  True = ("#101010","#050505","#202020")
defaultWorkspaceColors False  True False = ("#404040","#202020","#b0b0b0")
defaultWorkspaceColors False  True  True = ("#101010","#050505","#505050")
defaultWorkspaceColors  True     _ False = ("#808020","#404010","#f0f0b0")
defaultWorkspaceColors  True     _  True = ("#404010","#202005","#909050")


defaultWindowColors :: Bool -- window's workspace has focus
                    -> Bool -- window's workspace name matches incremental search
                    -> Bool -- window's workspace the current one
                    -> Bool -- window is urgent
                    -> Bool -- window has focus
                    -> (String, String) -- window border and background color

defaultWindowColors   wsf     m     c     u  True = ("#802020", snd $ defaultWindowColors wsf m c u False)

defaultWindowColors False False False False     _ = ("#111111","#060606")
defaultWindowColors False False False  True     _ = ("#802020","#401010")
defaultWindowColors False False  True False     _ = ("#101010","#050505")
defaultWindowColors False False  True  True     _ = ("#401010","#200505")
defaultWindowColors False  True False False     _ = ("#202080","#101040")
defaultWindowColors False  True False  True     _ = ("#802080","#401040")
defaultWindowColors False  True  True False     _ = ("#101040","#100520")
defaultWindowColors False  True  True  True     _ = ("#401040","#200520")

defaultWindowColors  True False False False     _ = ("#208020","#104010")
defaultWindowColors  True False False  True     _ = ("#808020","#404010")
defaultWindowColors  True False  True False     _ = ("#104010","#052005")
defaultWindowColors  True False  True  True     _ = ("#404010","#202005")
defaultWindowColors  True  True False False     _ = ("#208080","#104040")
defaultWindowColors  True  True False  True     _ = ("#808080","#404040")
defaultWindowColors  True  True  True False     _ = ("#104040","#102020")
defaultWindowColors  True  True  True  True     _ = ("#404040","#202020")


pagerPaint ::
  PagerConfig
  -> RhombusConfig
  -> Display
  -> Drawable
  -> GC
  -> WorkspaceId
  -> Rectangle
  -> Bool
  -> Bool
  -> Bool
  -> X ()
pagerPaint pc rc d p gc t r focus match current = do
    ss <- gets windowset

    let x = rect_x r
        y = rect_y r

    urgents <- readUrgents
    let foci = map W.focus $ catMaybes $ map W.stack $ W.workspaces ss

    let color = pc_windowColors pc focus match current -- :: Bool -> (String, String)
        (_, _, _fg_color) = pc_workspaceColors pc focus match current

    fg_color <- stringToPixel d _fg_color

    let r = screenRect $ W.screenDetail $ W.current ss
    let a = fi (rect_width r) / fi (rect_height r)
    let scale = fi (rc_cellwidth rc) / fi (rect_width r)

    -- TODO whenNothing print error
    whenJust (findWorkspace t ss) $ \ ws -> do
        whenJust (W.stack ws) $ \ s ->
            withDisplay $ \ d -> io $ do

                let color' w = color (w `elem` urgents) (w `elem` foci)

                -- TODO painting of floating windows is broken
                mapM_ (drawMiniWindow d p gc x y color' scale) (W.down s)
                drawMiniWindow d p gc x y color' scale (W.focus s)
                mapM_ (drawMiniWindow d p gc x y color' scale) (W.up s)

drawMiniWindow
    :: RealFrac a
    => Display
    -> Drawable
    -> GC
    -> Position
    -> Position
    -> (Window -> (String, String))
    -> a
    -> Window
    -> IO ()
drawMiniWindow d p gc ox oy color s win = do
    let scale x = round $ fi x * s

    wa <- getWindowAttributes d win

    let x = ox + (scale $ wa_x wa)
        y = oy + (scale $ wa_y wa)
        w = (scale $ wa_width wa)
        h = (scale $ wa_height wa)

    let (fg, bg) = color win

    fg' <- stringToPixel d fg
    bg' <- stringToPixel d bg

    setForeground d gc bg'
    fillRectangle d p gc (x + 1) (y + 1) (w - 2) (h - 2)

    setForeground d gc fg'
    drawLines d p gc
        [ Point x y
        , Point (fi w - 1) 0
        , Point 0 (fi h - 2)
        , Point (- fi w + 1) 0
        , Point 0 (- fi h + 2)
        ]
        coordModePrevious



-- TODO externalize findWorkspace
findWorkspace :: (Eq i) => i -> W.StackSet i l a sid sd -> Maybe (W.Workspace i l a)
findWorkspace t ss = find ((==)t . W.tag) (W.workspaces ss)
