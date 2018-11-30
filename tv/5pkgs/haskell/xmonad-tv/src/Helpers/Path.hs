module Helpers.Path where

import qualified Data.List
import qualified System.Directory
import qualified System.IO.Unsafe


findExecutable :: String -> FilePath
findExecutable =
    System.IO.Unsafe.unsafePerformIO . find
  where
    find name =
        maybe failure id <$> System.Directory.findExecutable name
      where
        failure = error (Data.List.intercalate " " [name, "not found"])
