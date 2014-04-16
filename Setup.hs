{-| This is a small harness to check the version of the Cabal library and
    build and run the real setup script, @DoSetup.hs@.
-}

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP
import System.Environment
import System.Exit
import System.Process

-- | Get Cabal major and minor version from the full package name
--
--   > versionParser "Cabal-1.16.0" = Just (1, 16)
versionParser :: ReadP (Int, Int)
versionParser = do
  string "Cabal-"
  major <- parseInt
  char '.'
  minor <- parseInt
  skipMany (char '.' >> parseInt)
  eof
  return (major, minor)

parseInt :: ReadP Int
parseInt = liftM read $ munch1 isDigit

getCabalVersion :: IO (Int, Int)
getCabalVersion = do
  s <- readProcess "ghc-pkg" ["--simple-output", "latest", "Cabal"] ""
  case words s of
    [w] -> case readP_to_S versionParser w
           of (version, ""):_ -> return version
              _               -> failed
    _   -> failed
  where
    failed = fail "Could not detect Cabal library version"

-- | Pick a CPP flag based on the version
cabalVersionCPPFlag :: (Int, Int) -> IO String
cabalVersionCPPFlag v
  | v == (1,14)                = return "-DCABAL_1_14"
  | v >= (1,16) && v <= (1,18) = return "-DCABAL_1_16"
  | v > (1,18)                 = do putStrLn "Warning: unsupported Cabal library version"
                                    return "-DCABAL_1_16"
  | otherwise                  = fail "Unrecognized Cabal library."

setupPath = "dist/setup/do-setup"

buildSetup = do
  cpp_flag <- cabalVersionCPPFlag =<< getCabalVersion

  ec <- rawSystem "ghc" ["--make", "DoSetup.hs", cpp_flag, "-o", setupPath]
  unless (ec == ExitSuccess) $
    fail "Error occured when building the setup script"

main = do
  buildSetup
  args <- getArgs
  ec <- rawSystem setupPath args
  exitWith ec