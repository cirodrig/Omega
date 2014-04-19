{-| This is a small harness to check the version of the Cabal library and
    build and run the real setup script, @DoSetup.hs@.
-}

import Control.Monad
import Data.Char
import Distribution.Simple.Utils(cabalVersion)
import Distribution.Version(Version(..))
import Text.ParserCombinators.ReadP
import System.Environment
import System.Exit
import System.Process

-- | Pick a CPP flag based on the Cabal library version
cabalVersionCPPFlag :: IO String
cabalVersionCPPFlag
  | v == (1,14)                = return "-DCABAL_1_14"
  | v >= (1,16) && v <= (1,18) = return "-DCABAL_1_16"
  | v > (1,18)                 = do putStrLn "Warning: unsupported Cabal library version"
                                    return "-DCABAL_1_16"
  | otherwise                  = fail "Unrecognized Cabal library"
  where
    v = case cabalVersion
        of Version {versionBranch = major : minor : _} -> (major, minor)

setupPath = "dist/setup/do-setup"

buildSetup = do
  cpp_flag <- cabalVersionCPPFlag

  ec <- rawSystem "ghc" ["--make", "-XCPP", cpp_flag,
                         "DoSetup.hs", "-o", setupPath]
  unless (ec == ExitSuccess) $
    fail "Error occured when building the setup script"

main = do
  buildSetup
  args <- getArgs
  ec <- rawSystem setupPath args
  exitWith ec