{ writeHaskellPackage }:

# Because `sed -n 's/.*\<ghc-options:\s\+\(.*\)/\1/p'` is too simple.
writeHaskellPackage "cabal-read" {
  executables.ghc-options = {
    extra-depends = ["Cabal"];
    text = /* haskell */ ''
      module Main (main) where
      import Data.List
      import Data.Maybe
      import Distribution.Compiler
      import Distribution.PackageDescription.Parsec
      import Distribution.Types.BuildInfo
      import Distribution.Types.CondTree
      import Distribution.Types.Executable
      import Distribution.Types.GenericPackageDescription
      import Distribution.Types.UnqualComponentName
      import Distribution.Verbosity
      import System.Environment
      main :: IO ()
      main = do
          [path, name] <- getArgs

          desc <- readGenericPackageDescription normal path

          case lookup (mkUnqualComponentName name) (condExecutables desc) of
            Just exe ->
              putStrLn . intercalate " " . fromMaybe [] . lookup GHC
                       . options . buildInfo . condTreeData $ exe

            Nothing ->
              error ("executable " <> name <> " not found in " <> path)
    '';
  };
}
