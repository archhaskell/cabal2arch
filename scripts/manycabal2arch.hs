-- |
-- Module    : manycabal2arch: batch creation of PKGBUILDs from Hackage tarball
-- Copyright : (c) Rémy Oudompheng 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
-- Stability : provisional
-- Portability:
--

import Distribution.ArchLinux.PkgBuild
import Distribution.ArchLinux.CabalTranslation
import Distribution.ArchLinux.HackageTranslation

import Distribution.PackageDescription
import Distribution.Text
import Text.PrettyPrint

import Data.List
import qualified Data.ByteString.Lazy as Bytes
import System.Directory
import System.FilePath

-- Input/output
import System.Environment
import System.IO
import System.Process
import System.Exit

import qualified Control.OldException as C

import Debug.Trace

import Paths_cabal2arch

exportPackage :: FilePath -> String -> GenericPackageDescription -> IO ()
exportPackage dot email p = do
  let q = preprocessCabal p
  case q of
    Nothing -> return ()
    Just p' -> do
      let (pkg, script) = cabal2pkg p'
          pkgname = trace ("Converting package " ++ arch_pkgname pkg) (arch_pkgname pkg)
      pkgbuild  <- getMD5 pkg
      let apkgbuild = AnnotatedPkgBuild { pkgBuiltWith = Just version, pkgHeader = comment, pkgBody = pkgbuild }
          rawpkgbuild = (render $ pkg2doc email apkgbuild) ++ "\n"

      createDirectoryIfMissing True (dot </> pkgname)
      writeFile (dot </> pkgname </> "PKGBUILD") rawpkgbuild
      case script of
        Nothing -> return ()
        Just s -> writeFile (dot </> pkgname </> pkgname ++ ".install") s

main :: IO ()
main = do
  argv <- getArgs
  x <- case argv of
    _:_:_:_ -> return ()
    _ -> help
  pkglist <- readFile (argv !! 0)
  tarball <- Bytes.readFile (argv !! 1)
  repo <- canonicalizePath (argv !! 2)
  email <- do
    r <- getEnvMaybe "ARCH_HASKELL"
    case r of
      Nothing -> do hPutStrLn stderr "Warning: ARCH_HASKELL environment variable not set. Set this to the maintainer contact you wish to use. \n E.g. 'Arch Haskell Team <arch-haskell@haskell.org>'"
                    return []
      Just s  -> return s
  let cabals = getSpecifiedCabalsFromTarball tarball (lines pkglist)
  mapM (exportPackage repo email) cabals
  return ()

-- Safe wrapper for getEnv                                                                                            
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe name = C.handle (const $ return Nothing) (Just `fmap` getEnv name)

comment :: String
comment = render $ vcat
 [ text "# Note: we list all package dependencies."
 , text "# Your package tool should understand 'provides' syntax"
 , text "#"
 , text "# Keep up to date on http://archhaskell.wordpress.com/"
 , text "#"]

-- | Given an abstract pkgbuild, run "makepkg -g" to compute md5
-- of source files (possibly cached locally), and modify the PkgBuild
-- accordingly.
--
getMD5 :: PkgBuild -> IO PkgBuild
getMD5 pkg = do
   putStrLn "Feeding the PKGBUILD to `makepkg -g`..."
   eres <- readProcessWithExitCode "makepkg" ["-g"] (render $ disp pkg)
   case eres of
       (ExitFailure _,_,err) -> do
            hPutStrLn stderr err
            hPutStrLn stderr $ "makepkg encountered an error while calculating MD5."
            return pkg
       (ExitSuccess,out,err) -> do
            -- s should be "md5sums=(' ... ')"
            hPutStrLn stderr err
            if "md5sums=('" `isPrefixOf` out
               then
                 let md5sum = takeWhile (\x -> x `elem` "0123456789abcdef") $ drop 10 out
                 in return pkg { arch_md5sum = ArchList [md5sum] }
               else do
                 hPutStrLn stderr $ "Incorrect output from makepkg."
                 return pkg

--
-- | Help file
--
help :: IO a
help = do
 hPutStrLn stderr $ unlines
    [ "Usage: manycabal2arch pkglist tarball destination"
    , ""
    , "  Generate a ABS-like tree from a list of packages and the"
    , "  uncompressed Hackage tarball"
    , ""
    , "Arguments: pkglist"
    , "              A file with lines of the form \"<package-name> <version-number>\""
    , "           tarball"
    , "              The path to 00-index.tar, for example"
    , "              $HOME/.cabal/packages/hackage.haskell.org/00-index.tar"
    , "           destination"
    , "              The directory where the repository will be created"
    ]
 exitWith ExitSuccess
