{-# LANGUAGE ScopedTypeVariables #-}

module THEnv.JSON where

import Data.Aeson (eitherDecode,FromJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Language.Haskell.TH.Syntax (Exp,Lift(lift),Q)
import THEnv (getCompileEnv)
import Control.Monad

getCompileEnvJSON :: (FromJSON a) => String -> Q a
getCompileEnvJSON name =
    either error (id :: a -> a) . eitherDecode . pack <$>  getCompileEnv name

getCompileEnvJSONExp ::
  forall proxy a. (FromJSON a, Lift a) => proxy a -> String -> Q Exp
getCompileEnvJSONExp _ =
    (lift :: a -> Q Exp) <=< getCompileEnvJSON
