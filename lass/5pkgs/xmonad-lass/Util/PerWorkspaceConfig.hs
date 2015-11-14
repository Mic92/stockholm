module Util.PerWorkspaceConfig
  ( WorkspaceConfig (..)
  , WorkspaceConfigs
  , switchToWorkspace
  , defaultWorkspaceConfig
  , perWorkspaceAction
  , perWorkspaceTermAction
--  , myLayoutHack
  )
where

import XMonad
import XMonad.Core (LayoutClass)
import Control.Monad (when)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

data WorkspaceConfig l =
  WorkspaceConfig
    { switchAction :: X ()
    , startAction  :: X ()
    , keyAction    :: X ()
    , termAction   :: X ()
    }

type WorkspaceConfigs l = M.Map WorkspaceId (WorkspaceConfig l)

defaultWorkspaceConfig = WorkspaceConfig
                             { switchAction = return ()
                             , startAction  = return ()
                             , keyAction    = return ()
                             , termAction   = spawn "urxvtc"
                             }

whenLookup wsId cfg a =
    when (M.member wsId cfg) (a $ cfg M.! wsId)

switchToWorkspace :: WorkspaceConfigs l -> WorkspaceId -> X ()
switchToWorkspace cfg wsId = do
  windows $ W.greedyView wsId
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ whenLookup wsId cfg startAction
  whenLookup wsId cfg switchAction

perWorkspaceAction :: WorkspaceConfigs l -> X ()
perWorkspaceAction cfg = withWindowSet $ \s -> whenLookup (W.currentTag s) cfg keyAction

perWorkspaceTermAction :: WorkspaceConfigs l -> X ()
perWorkspaceTermAction cfg = withWindowSet $ \s -> case M.lookup (W.currentTag s) cfg of
                                                       Just x -> termAction x
                                                       _      -> termAction defaultWorkspaceConfig
