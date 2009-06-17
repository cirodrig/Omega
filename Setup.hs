
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import qualified Distribution.Verbosity as Verbosity
import System.Cmd
import System.Directory
import System.Exit(ExitCode(..))
import System.IO
import System.FilePath((</>))
import System.Process

-- Mimic the && command of 'sh'
(>&&>) :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd1 >&&> cmd2 = do
  rc <- cmd1
  case rc of
    ExitSuccess   -> cmd2
    ExitFailure _ -> return rc

-- We will call 'autoconf' and 'make'
autoconfProgram = simpleProgram "autoconf"
makeProgram = simpleProgram "make"

-------------------------------------------------------------------------------
-- Configuration

configureOmega pkgDesc flags = do
  -- Run Cabal configure
  lbi <- confHook simpleUserHooks pkgDesc flags

  let verb = fromFlagOrDefault Verbosity.normal $ configVerbosity flags
      cfg = withPrograms lbi 

      runAutoconf = do rawSystemProgramConf verb autoconfProgram cfg []
                       return ExitSuccess
      
  -- Run autoconf configure
  runAutoconf >&&> runConfigure lbi

  return lbi

    where
      -- Run 'configure' with the extra arguments that were passed to
      -- Setup.hs
      runConfigure lbi = do
        currentDir <- getCurrentDirectory

        let opts = autoConfigureOptions lbi
            configProgramName = currentDir </> "configure"

        rawSystem configProgramName opts

-- Configuration: extract options to pass to 'configure'
autoConfigureOptions :: LocalBuildInfo -> [String]
autoConfigureOptions localBuildInfo = [libdirs, includedirs]
    where
      libraryDescr = case library $ localPkgDescr localBuildInfo
                     of Nothing -> error "Library description is missing"
                        Just l -> l

      buildinfo = libBuildInfo libraryDescr

      -- Create a string "-L/usr/foo -L/usr/bar"
      ldflagsString =
          intercalate " " ["-L" ++ dir | dir <- extraLibDirs buildinfo]

      libdirs = "LDFLAGS=" ++ ldflagsString

      -- Create a string "-I/usr/foo -I/usr/bar"
      cppflagsString =
          intercalate " " ["-I" ++ dir | dir <- includeDirs buildinfo]
      includedirs = "CPPFLAGS=" ++ cppflagsString

-------------------------------------------------------------------------------
-- Building

buildOmega pkgDesc lbi userhooks flags = do
  -- Do default build procedure for hs files
  buildHook simpleUserHooks pkgDesc lbi userhooks flags

  -- Get 'ar' program
  let verb = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags
  (arPgm, _) <- requireProgram verb arProgram AnyVersion (withPrograms lbi)

  -- Build the C++ source file
  rawSystemProgramConf verb makeProgram (withPrograms lbi) []

  -- Add the object file to libraries
  let objName = "build" </> "C_omega.o"
  let pkgId   = package $ localPkgDescr lbi

  let addStaticObjectFile objName libName =
          rawSystemProgram verb arPgm ["r", libName, objName]

  when (withVanillaLib lbi) $
       let libName = buildDir lbi </> mkLibName pkgId
       in addStaticObjectFile objName libName

  when (withProfLib lbi) $
       let libName = buildDir lbi </> mkProfLibName pkgId
       in addStaticObjectFile objName libName

  when (withSharedLib lbi) $
       die "Sorry, this package is not set up to build shared libraries"

  return ()

-------------------------------------------------------------------------------
-- Hooks

hooks =
    simpleUserHooks
    { hookedPrograms = [arProgram, autoconfProgram, makeProgram]
    , confHook = configureOmega
    , buildHook = buildOmega
    }

main = defaultMainWithHooks hooks
