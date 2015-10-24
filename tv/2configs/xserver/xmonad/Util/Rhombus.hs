module Util.Rhombus
    ( defaultRhombusConfig
    , MatchMethod(..)
    , rhombus
    , RhombusConfig(..)
    , RhombusState(..)
    ) where

import Control.Monad ( forM_, zipWithM_ )
import Data.Char
import Data.List
import Data.Ord
import Data.Map ( fromList )
import Data.Maybe ( isJust, fromJust )
import XMonad
import XMonad.StackSet hiding ( filter )
import XMonad.Util.Font
import XMonad.Util.Image ( drawIcon )
import XMonad.Util.XUtils

import Util.Debunk
import Util.Submap
import Util.XUtils
import Util.Font


data MatchMethod = MatchInfix | MatchPrefix

data RhombusConfig = RhombusConfig
    { rc_font           :: String
    , rc_cellwidth      :: Dimension
    , rc_margin         :: Dimension
    , rc_matchmethod    :: MatchMethod
    , rc_wrap           :: Bool
    , rc_colors         :: Bool -> Bool -> Bool -> (String, String, String)
    , rc_paint          :: RhombusConfig -> Display -> Pixmap -> GC -> String -> Rectangle -> Bool -> Bool -> Bool -> X ()
    }


-- TODO currently xft is broken
defaultRhombusConfig = RhombusConfig "xft:Sans-8" 100 0 MatchInfix True stupidColors noPaint
    where
    stupidColors _ _ _ = ("red", "magenta", "yellow")
    noPaint _ _ _ _ _ _ _ _ _ = return ()


data RhombusState = RhombusState
    { rs_window     :: Window
    , rs_search     :: String
    , rs_font       :: XMonadFont
    , rs_focus      :: (Position, Position)
    , rs_strings    :: [String]
    }


reachableCoords :: RhombusState -> [(Position, Position)]
reachableCoords RhombusState{rs_strings=xs} = take (length xs) wave


matchingReachableCoords :: RhombusConfig -> RhombusState -> [(Position, Position)]
matchingReachableCoords rc rs =
    snd $ unzip
        $ filter (isXOf (rc_matchmethod rc) (rs_search rs) . fst)
        $ zip (rs_strings rs) (reachableCoords rs)


match :: MatchMethod -> String -> [String] -> Maybe String
match m s ws = do
    let cands = filter (isXOf m s) ws
    if length cands == 1
        then Just $ head cands
        else Nothing

rhombus :: RhombusConfig -> (String -> X ()) -> [String] -> X ()
rhombus rc viewFunc as = withGrabbedKeyboard $ do
    rs <- newRhombus rc as
    --redraw rc rs
    showWindow (rs_window rs)
    rhombusMode viewFunc rc rs


rhombusMode :: (String -> X ()) -> RhombusConfig -> RhombusState -> X ()
rhombusMode viewFunc rc rs =
    case match (rc_matchmethod rc) (rs_search rs) (init $ rs_strings rs) of
        Nothing -> redraw rc rs >> submapString def keys
        Just i -> removeRhombus rs >> viewFunc i
    where
    def (ch:[]) | isPrint ch =
        incSearchPushChar ch rs >>= rhombusMode viewFunc rc

    def _ =
        failbeep >> rhombusMode viewFunc rc rs

    keys = fromList $
        [ ((0   , xK_BackSpace  ), incSearchPopChar rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Escape     ), removeRhombus rs)
        , ((0   , xK_Menu       ), removeRhombus rs)
        , ((0   , xK_Left       ), goto rc (-1, 0) rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Right      ), goto rc ( 1, 0) rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Up         ), goto rc ( 0,-1) rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Down       ), goto rc ( 0, 1) rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Tab        ), gotoNextMatch rc rs >>= rhombusMode viewFunc rc)
        , ((_S  , xK_Tab        ), gotoPrevMatch rc rs >>= rhombusMode viewFunc rc)
        , ((0   , xK_Return     ), removeRhombus rs >> return (selectFocused rs) >>= viewFunc)
        ]

    _S = shiftMask


-- TODO make failbeep configurable
failbeep = spawn "beep -l 100 -f 500"


goto :: RhombusConfig -> (Position, Position) -> RhombusState -> X RhombusState
goto RhombusConfig{rc_wrap=True}  xy rs = maybe (failbeep >> return rs) return $ wrapFocus xy rs
goto RhombusConfig{rc_wrap=False} xy rs = maybe (failbeep >> return rs) return $ moveFocus xy rs


moveFocus :: (Position, Position) -> RhombusState -> Maybe RhombusState
moveFocus (dx, dy) rs@RhombusState{rs_focus=(x,y)} = do
    let focus' = (x + dx, y + dy)
    if elem focus' (reachableCoords rs)
        then Just rs { rs_focus = focus' }
        else Nothing


wrapFocus :: (Position, Position) -> RhombusState -> Maybe RhombusState

wrapFocus (0, dy) rs@RhombusState{rs_focus=focus} = do
    let column = sortBy (comparing snd) $ filter ((==) (fst focus) . fst) (reachableCoords rs)
    i <- elemIndex focus column
    return rs { rs_focus = column `modIndex` (i + fromIntegral dy) }

wrapFocus (dx, 0) rs@RhombusState{rs_focus=focus} = do
    let column = sortBy (comparing fst) $ filter ((==) (snd focus) . snd) (reachableCoords rs)
    i <- elemIndex focus column
    return rs { rs_focus = column `modIndex` (i + fromIntegral dx) }

wrapFocus _ _ = Nothing


gotoPrevMatch :: RhombusConfig -> RhombusState -> X RhombusState
gotoPrevMatch rc rs@RhombusState{rs_focus=focus} = do
    case reverse (matchingReachableCoords rc rs) of
        [] -> failbeep >> return rs
        xs -> return rs
            { rs_focus = maybe (head xs)
                               (modIndex xs . (+1))
                               (focus `elemIndex` xs)
            }


gotoNextMatch :: RhombusConfig -> RhombusState -> X RhombusState
gotoNextMatch rc rs@RhombusState{rs_focus=focus} = do
    case matchingReachableCoords rc rs of
        [] -> failbeep >> return rs
        xs -> return rs
            { rs_focus = maybe (head xs)
                               (modIndex xs . (+1))
                               (focus `elemIndex` xs)
            }


selectFocused :: RhombusState -> String
selectFocused rs =
    -- TODO the rhombus must never "focus" something inexistent
    fromJust $ lookup (rs_focus rs) $ zip wave (rs_strings rs)


incSearchPushChar :: Char -> RhombusState -> X RhombusState
incSearchPushChar c rs = return rs { rs_search = rs_search rs ++ [c] }


incSearchPopChar :: RhombusState -> X RhombusState

-- only rubout if we have at least one char
incSearchPopChar rs@RhombusState{rs_search=xs@(_:_)} =
    return rs { rs_search = init xs }

incSearchPopChar rs = return rs


redraw :: RhombusConfig -> RhombusState -> X ()
redraw rc rs = do
    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss

    -- TODO this let is duplicated in newRhombus
    let scale x = x * cell_w `div` s_width -- TODO use bw
        cell_w  = rc_cellwidth rc
        cell_h  = scale s_height

        -- txy is the top-left corner of the first (center) cell
        -- XXX div and (-) are not distributive
        --     we could round $ (s_* - cell_*) / 2, though...
        tx = fi $ s_width  `div` 2 - cell_w `div` 2
        ty = fi $ s_height `div` 2 - cell_h `div` 2

        margin = rc_margin rc

        -- dxy are the outer cell dimensions (i.e. including the border)
        dx = fi $ cell_w + 2 + margin
        dy = fi $ cell_h + 2 + margin

        paint = rc_paint rc
        xmf   = rs_font rs
        tags  = rs_strings rs
        --currentTag = last tags

    withDisplay $ \ d -> do
        -- XXX we cannot use withPixmapAndGC because rc_paint is an X monad
        p <- io $ createPixmap d (rs_window rs) s_width s_height (defaultDepthOfScreen $ defaultScreenOfDisplay d)
        g <- io $ createGC d p

        -- TODO fixme
        color_black <- stringToPixel d "black"

        forZipWithM_ tags (reachableCoords rs) $ \ tag oxy@(ox, oy) -> do

            let focus   = oxy == rs_focus rs
                match   = isXOf (rc_matchmethod rc) (rs_search rs) tag
                current = tag == last tags
                (_b_color, _bg_color, _fg_color) = rc_colors rc focus match current
                --cell_x = (ox * dx) + x - fi (cell_w `div` 2)
                --cell_y = (oy * dy) + y - fi (cell_h `div` 2)
                cell_x = (ox * dx) + tx + 1
                cell_y = (oy * dy) + ty + 1

            b_color <- stringToPixel d _b_color
            bg_color <- stringToPixel d _bg_color
            fg_color <- stringToPixel d _fg_color

            -- draw background
            io $ setForeground d g bg_color
            io $ fillRectangle d p g cell_x cell_y cell_w cell_h

            -- draw border
            io $ setForeground d g b_color
            io $ drawLines d p g
                    [ Point (cell_x - 1) (cell_y - 1)
                    , Point (fi cell_w + 1) 0
                    , Point 0 (fi cell_h + 1)
                    , Point (-(fi cell_w + 1)) 0
                    , Point 0 (-(fi cell_h + 1))
                    ]
                    coordModePrevious

            -- custom draw
            paint rc d p g tag (Rectangle cell_x cell_y cell_w cell_h) focus match current

            -- paint text
            -- TODO custom paint text?
            -- TODO withCopyArea
            io $ withPixmapAndGC d p s_width s_height (defaultDepthOfScreen $ defaultScreenOfDisplay d) $ \ f_pm f_gc -> do
                withPixmapAndGC d f_pm s_width s_height 1 $ \ clip_mask clip_gc -> do
                    setForeground d clip_gc 0
                    setBackground d clip_gc 0
                    fillRectangle d clip_mask clip_gc 0 0 s_width s_height
                    setForeground d clip_gc 1

                    let r = Rectangle cell_x cell_y cell_w cell_h

                    printStringCentered d clip_mask xmf clip_gc r tag

                    setForeground d f_gc fg_color
                    setBackground d f_gc color_black -- TODO

                    printStringCentered d f_pm xmf f_gc r tag

                    setClipMask d f_gc clip_mask

                    copyArea d f_pm p f_gc 0 0 s_width s_height 0 0

        io $ copyArea d p (rs_window rs) g 0 0 s_width s_height 0 0
        io $ freePixmap d p
        io $ freeGC d g


newRhombus :: RhombusConfig -> [String] -> X RhombusState
newRhombus rc tags = do
    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss
        (_, def_win_bg, _) = rc_colors rc False True False

    -- TODO this let is duplicated in redraw
    let scale x = x * cell_w `div` s_width -- TODO use bw
        cell_w  = rc_cellwidth rc
        cell_h  = scale s_height

        -- TODO don't delete this let but use it instead of s_{width,height}
        -- (xcoords, ycoords) = unzip $ take (length tags) wave -- this is reachableCoords
        -- win_width  = (maximum xcoords - minimum xcoords) * dx
        -- win_height = (maximum ycoords - minimum ycoords) * dy

        -- txy is the top-left corner of the first (center) cell
        -- XXX div and (-) are not distributive
        --     we could round $ (s_* - cell_*) / 2, though...
        tx = fi $ s_width  `div` 2 - cell_w `div` 2
        ty = fi $ s_height `div` 2 - cell_h `div` 2

        margin = rc_margin rc

        -- dxy are the outer cell dimensions (i.e. including the border)
        dx = fi $ cell_w + 2 + margin
        dy = fi $ cell_h + 2 + margin

    fn <- initXMF (rc_font rc)
    win <- createNewWindow (Rectangle 0 0 s_width s_height) Nothing def_win_bg True

    withDisplay $ \ d ->
        io $ shapeWindow d win $ \ p g ->
            forZipWithM_ tags wave $ \ _ (ox, oy) ->
                fillRectangle d p g (tx + ox * dx) (ty + oy * dy) (fi cell_w + 2) (fi cell_h + 2)

    return $ RhombusState win "" fn (0,0) tags


removeRhombus :: RhombusState -> X ()
removeRhombus (RhombusState w _ fn _ _) = do
    deleteWindow w
    releaseXMF fn

wave :: [(Position, Position)]
wave = zip (0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])) (concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..])
    where
        wave1 = 0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])
        wave2 = concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..]

commonPrefix (x:xs) (y:ys) | x == y = x:commonPrefix xs ys
commonPrefix _ _ = []


isXOf :: MatchMethod -> String -> String -> Bool
isXOf MatchInfix  = isInfixOf
isXOf MatchPrefix = isPrefixOf


findXIndex :: (Eq a) => MatchMethod -> [a] -> [a] -> Maybe Int
findXIndex MatchInfix  = findInfixIndex
findXIndex MatchPrefix = findPrefixIndex


findInfixIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findInfixIndex needle haystack
    = (\x -> if null x then Nothing else Just (fst $ head x))
      . dropWhile (\(_,x) -> not $ isPrefixOf needle x)
        $ zip [0..] (tails haystack)


findPrefixIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findPrefixIndex needle haystack =
    if isPrefixOf needle haystack
        then Just 0
        else Nothing


modIndex :: Integral i => [a] -> i -> a
modIndex xs i = xs `genericIndex` (i `mod` genericLength xs)


forZipWithM_ a b f = zipWithM_ f a b


withGrabbedKeyboard f = do
    XConf { theRoot = root, display = d } <- ask
    catchX (io (grabKeyboard d root False grabModeAsync grabModeAsync currentTime) >> f)
           (return ())
    io $ ungrabKeyboard d currentTime
