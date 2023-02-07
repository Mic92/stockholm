{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module XMonad.Hooks.EwmhDesktops.Extra where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid (All)
import Data.Tuple.Extra (both)
import Graphics.X11.EWMH (getDesktopNames, setDesktopNames)
import Graphics.X11.EWMH.Atom (_NET_DESKTOP_NAMES)
import Graphics.X11.Xlib.Display.Extra (withDefaultDisplay)
import XMonad hiding (workspaces)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, removeEmptyWorkspaceByTag)
import XMonad.StackSet (mapWorkspace, tag, workspaces)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified XMonad


ewmhExtra :: XConfig a -> IO (XConfig a)
ewmhExtra c = do
    -- XMonad.Hooks.EwmhDesktops.setDesktopViewport uses _NET_DESKTOP_VIEWPORT
    -- only if it exists.  This seems to be a harmless issue, but by creating
    -- the atom here, we suppress the error message:
    --
    --    xmonad: X11 error: BadAtom (invalid Atom parameter),
    --    request code=18, error code=5
    --
    _ <-
      withDefaultDisplay $ \dpy -> internAtom dpy "_NET_DESKTOP_VIEWPORT" False

    initialWorkspaces <-
      Data.Maybe.fromMaybe (XMonad.workspaces def)
        <$> withDefaultDisplay getDesktopNames

    return
      c { handleEventHook = ewmhDesktopsExtraEventHook <> handleEventHook c
        , rootMask = rootMask c .|. propertyChangeMask
        , XMonad.workspaces = initialWorkspaces
        }

ewmhDesktopsExtraEventHook :: Event -> X All
ewmhDesktopsExtraEventHook = \case
    PropertyEvent{ev_window, ev_atom} -> do
      r <- asks theRoot
      when (ev_window == r && ev_atom == _NET_DESKTOP_NAMES) $
        withDisplay $ \dpy -> do
          sort <- getSortByIndex

          oldNames <- gets $ map tag . sort . workspaces . windowset
          newNames <- fromMaybe oldNames <$> io (getDesktopNames dpy)

          let
            (renamesFrom, renamesTo) = both Set.fromList $ unzip renames

            renames = go oldNames newNames where
              go old@(headOld : tailOld) new@(headNew : tailNew) = do
                let
                  deleteOld = Set.member headOld deleteNameSet
                  createNew = Set.member headNew createNameSet

                if
                  | headOld == headNew ->
                    -- assert (not deleteOld && not createNew)
                    go tailOld tailNew

                  | deleteOld && createNew ->
                    (headOld, headNew) :
                    go tailOld tailNew

                  | deleteOld ->
                    go tailOld new

                  | createNew ->
                    go old tailNew

                  | otherwise ->
                    -- assert (headOld == headNew)
                    go tailOld tailNew

              go _ _ = []

            oldNameSet = Set.fromList oldNames
            newNameSet = Set.fromList newNames
            deleteNameSet = Set.difference oldNameSet newNameSet
            createNameSet = Set.difference newNameSet oldNameSet

            deleteNames = Set.toAscList $
                            Set.difference deleteNameSet renamesFrom
            createNames = Set.toAscList $
                            Set.difference createNameSet renamesTo

          mapM_ addHiddenWorkspace createNames
          mapM_ removeEmptyWorkspaceByTag deleteNames
          when (not (null renames)) $ do
            let
              renameMap = Map.fromList renames
              rename w =
                case Map.lookup (tag w) renameMap of
                  Just newName -> w { tag = newName }
                  Nothing -> w

            modifyWindowSet $ mapWorkspace rename

          names <- gets $ map tag . sort . workspaces . windowset

          when (names /= newNames) $ do
            trace $ "setDesktopNames " <> show names
            io (setDesktopNames names dpy)

      mempty

    _ ->
      mempty
