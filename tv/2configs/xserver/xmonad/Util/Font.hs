{-# LANGUAGE CPP #-}
module Util.Font
    ( printStringCentered
    , printStringXMF'
    ) where

import XMonad
import XMonad.Util.Font


printStringCentered :: (Functor m, MonadIO m)
                    => Display -> Drawable -> XMonadFont
                    -> GC -> Rectangle -> String
                    -> m ()
printStringCentered d p xmf gc r s = do
    let x = rect_x r
        y = rect_y r
        w = rect_width r
        h = rect_height r

    text_w <- textWidthXMF d xmf s
    (text_ascent, _) <- textExtentsXMF xmf s

    let text_x = x + round ((fi w - fi text_w) / 2)
        text_y = y + round ((fi h + fi text_h) / 2)
        text_h = text_ascent

    printStringXMF' d p xmf gc "" "" text_x text_y s


-- from xmonad-contrib's XMonad.Util.Font, (c) 2007 Andrea Rossato and Spencer Janssen
printStringXMF' :: (Functor m, MonadIO m) => Display -> Drawable -> XMonadFont -> GC -> String -> String
            -> Position -> Position -> String  -> m ()
printStringXMF' d p (Core fs) gc fc bc x y s = io $ do
    setFont d gc $ fontFromFontStruct fs
    --tv [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    --tv setForeground d gc fc'
    --tv setBackground d gc bc'
    drawImageString d p gc x y s
printStringXMF' d p (Utf8 fs) gc fc bc x y s = io $ do
    --tv [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    --tv setForeground d gc fc'
    --tv setBackground d gc bc'
    io $ wcDrawImageString d p fs gc x y s
#ifdef XFT
printStringXMF' dpy drw fs@(Xft font) gc fc bc x y s = do
  let screen   = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual   = defaultVisualOfScreen screen
  --tv bcolor <- stringToPixel dpy bc
  (a,d)  <- textExtentsXMF fs s
  gi <- io $ xftTextExtents dpy font s
  --tv io $ setForeground dpy gc bcolor
  io $ fillRectangle dpy drw gc (x - fi (xglyphinfo_x gi))
                                (y - fi a)
                                (fi $ xglyphinfo_xOff gi)
                                (fi $ a + d)
  io $ withXftDraw dpy drw visual colormap $
         \draw -> withXftColorName dpy visual colormap fc $
                   \color -> xftDrawString draw color font x y s
#endif





-- --my_printStringXMF :: (Functor m, MonadIO m) => Display -> Drawable -> XMonadFont -> GC -> String -> String
-- --            -> Position -> Position -> String  -> m ()
-- my_printStringXMF (Core fs) d p gc x y s = do
--     setFont d gc $ fontFromFontStruct fs
--     -- [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
--     -- setForeground d gc fc'
--     -- setBackground d gc bc'
--     drawImageString d p gc x y s
-- my_printStringXMF (Utf8 fs) d p gc x y s = do
--     -- [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
--     -- setForeground d gc fc'
--     -- setBackground d gc bc'
--     wcDrawImageString d p fs gc x y s
-- #ifdef XFT
-- my_printStringXMF dpy drw fs@(Xft font) gc fc bc x y s = do
--   let screen   = defaultScreenOfDisplay dpy
--       colormap = defaultColormapOfScreen screen
--       visual   = defaultVisualOfScreen screen
--   bcolor <- stringToPixel dpy bc
--   (a,d)  <- textExtentsXMF fs s
--   gi <- io $ xftTextExtents dpy font s
--   io $ setForeground dpy gc bcolor
--   io $ fillRectangle dpy drw gc (x - fromIntegral (xglyphinfo_x gi))
--                                 (y - fromIntegral a)
--                                 (fromIntegral $ xglyphinfo_xOff gi)
--                                 (fromIntegral $ a + d)
--   io $ withXftDraw dpy drw visual colormap $
--          \draw -> withXftColorName dpy visual colormap fc $
--                    \color -> xftDrawString draw color font x y s
-- #endif



-- --textWidthXMF :: MonadIO m => Display -> XMonadFont -> String -> m Int
-- my_textWidthXMF _   (Utf8 fs) s = return $ fromIntegral $ wcTextEscapement fs s
-- my_textWidthXMF _   (Core fs) s = return $ fromIntegral $ textWidth fs s
-- #ifdef XFT
-- my_TextWidthXMF dpy (Xft xftdraw) s = liftIO $ do
--     gi <- xftTextExtents dpy xftdraw s
--     return $ xglyphinfo_xOff gi
-- #endif
-- 
-- my_textExtentsXMF :: MonadIO m => XMonadFont -> String -> m (Int32,Int32)
-- my_textExtentsXMF (Utf8 fs) s = do
--   let (_,rl)  = wcTextExtents fs s
--       ascent  = fromIntegral $ - (rect_y rl)
--       descent = fromIntegral $ rect_height rl + (fromIntegral $ rect_y rl)
--   return (ascent, descent)
-- my_textExtentsXMF (Core fs) s = do
--   let (_,a,d,_) = textExtents fs s
--   return (a,d)
-- #ifdef XFT
-- my_textExtentsXMF (Xft xftfont) _ = io $ do
--   ascent  <- fromIntegral `fmap` xftfont_ascent  xftfont
--   descent <- fromIntegral `fmap` xftfont_descent xftfont
--   return (ascent, descent)
-- #endif
