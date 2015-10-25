module Util.XUtils
    ( shapeWindow
    , withGC
    , withPixmap
    , withPixmapAndGC
    ) where

import Control.Exception ( bracket )
import Foreign.C.Types ( CInt )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xshape


shapeWindow :: Display -> Window -> (Pixmap -> GC -> IO ()) -> IO ()
shapeWindow d w f = do
    wa <- getWindowAttributes d w

    let width = fromIntegral $ wa_width wa
        height = fromIntegral $ wa_height wa

    withPixmapAndGC d w width height 1 $ \ p g -> do

        setForeground d g 0
        fillRectangle d p g 0 0 width height

        setForeground d g 1

        f p g

        xshapeCombineMask d w shapeBounding 0 0 p shapeSet


withGC :: Display -> Drawable -> (GC -> IO ()) -> IO ()
withGC d p =
    bracket (createGC d p) (freeGC d)


withPixmap :: Display -> Drawable -> Dimension -> Dimension -> CInt -> (Pixmap -> IO ()) -> IO ()
withPixmap d p w h depth =
    bracket (createPixmap d p w h depth) (freePixmap d)


withPixmapAndGC :: Display -> Drawable -> Dimension -> Dimension -> CInt -> (Pixmap -> GC -> IO ()) -> IO ()
withPixmapAndGC d w width height depth f =
    withPixmap d w width height depth $ \ p ->
        withGC d p $ \ g -> f p g
