{-# LANGUAGE TemplateHaskell #-}
module THEnv
    (
    -- * Compile-time configuration
      lookupCompileEnv
    , lookupCompileEnvExp
    , getCompileEnv
    , getCompileEnvExp
    , fileAsString
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Environment (getEnvironment)

-- Functions that work with compile-time configuration

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key `liftM` runIO getEnvironment

-- | Looks up a compile-time environment variable. The result is a TH
-- expression of type @Maybe String@.
lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnvExp = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnv
    -- We need to explicly type the result so that things like `print Nothing`
    -- work.

-- | Looks up an compile-time environment variable and fail, if it's not
-- present.
getCompileEnv :: String -> Q String
getCompileEnv key =
  lookupCompileEnv key >>=
  maybe (fail $ "Environment variable " ++ key ++ " not defined") return

-- | Looks up an compile-time environment variable and fail, if it's not
-- present. The result is a TH expression of type @String@.
getCompileEnvExp :: String -> Q Exp
getCompileEnvExp = lift <=< getCompileEnv

-- | Loads the content of a file as a string constant expression.
-- The given path is relative to the source directory.
fileAsString :: FilePath -> Q Exp
fileAsString = do
  -- addDependentFile path -- works only with template-haskell >= 2.7
  stringE . T.unpack . T.strip <=< runIO . T.readFile
