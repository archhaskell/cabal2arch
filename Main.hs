{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE BangPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- |
-- Module    : cabal2arch: convert cabal packages to Arch Linux PKGBUILD format
-- Copyright : (c) Don Stewart, 2008 .. 2010
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--

-- TODO: if build-type: Configure, accurate C library dependecies
-- require downloading the source, and running configure
--
-- C libraries are dynamically linked, should be listed in depends,
-- rather than makedepends

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Utils hiding (die)
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import Distribution.Compiler
import Distribution.System

-- from the archlinux package:
import Distribution.ArchLinux.PkgBuild
import Distribution.ArchLinux.CabalTranslation

import Control.Monad
import Control.Concurrent
import qualified Control.OldException as C

import Data.List
import qualified Data.Map as M
import Debug.Trace

import Text.PrettyPrint

import Paths_cabal2arch

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

comment :: String
comment = render $ vcat
 [ text "# Note: we list all package dependencies."
 , text "# Your package tool should understand 'provides' syntax"
 , text "#"
 , text "# Keep up to date on http://archhaskell.wordpress.com/"
 , text "#"]

main :: IO ()
main =
 C.bracket
   -- We do all our work in a temp directory
  (do cwd  <- getCurrentDirectory
      etmp <- myReadProcess "mktemp" ["-d"] []
      case etmp of
        Left _  -> die "Unable to create temp directory"
        Right d -> do
            let dir = makeValid (init d) -- drop newline
            setCurrentDirectory dir
            return (dir, cwd))

   -- Always remember to clean up
  (\(d,cwd) -> do
            setCurrentDirectory cwd
            removeDirectoryRecursive d)

   -- Now, get to work:
  $ \(tmp,cwd) -> do

   do x <- getArgs
      case x of
           ["--help"] -> help
           ["-h"]     -> help
           _          -> return ()
   email     <- do
       r <- getEnvMaybe "ARCH_HASKELL"
       case r of
            Nothing -> do hPutStrLn stderr "Warning: ARCH_HASKELL environment variable not set. Set this to the maintainer contact you wish to use. \n E.g. 'Arch Haskell Team <arch-haskell@haskell.org>'"
                          return []
            Just s  -> return s

   cabalfile <- findCabalFile cwd tmp
   hPutStrLn stderr $ "Using " ++ cabalfile

   cabalsrc  <- readPackageDescription normal cabalfile

   -- Create a package description with all configurations resolved.
   let e_finalcabalsrc = finalizePackageDescription
        []
        (const True)  -- could check against prefered pkgs....
        (Platform X86_64 buildOS) -- linux/x86_64
        (CompilerId GHC (Version [6,10,3] []))

        -- now constrain it to solve in the context of a modern ghc only
        corePackages
        cabalsrc

   finalcabal <- case e_finalcabalsrc of
        Left deps     -> die $ "Unresolved dependencies: " ++show deps
        Right (pkg,_) ->
            return $ pkg { buildDepends = removeCoreFrom (buildDepends pkg) }

   let (pkgbuild', hooks) = cabal2pkg finalcabal

   pkgbuild  <- getMD5 pkgbuild'
   let apkgbuild = AnnotatedPkgBuild { pkgBuiltWith = Just version, pkgHeader = comment, pkgBody = pkgbuild }
       doc = pkg2doc email apkgbuild
       dir = arch_pkgname pkgbuild

   setCurrentDirectory cwd
   createDirectoryIfMissing False dir
   setCurrentDirectory dir

   writeFile "PKGBUILD" (render doc ++ "\n")

   -- print pkgname.install
   case hooks of
        Nothing -> return ()
        Just i  -> writeFile (install_hook_name (arch_pkgname pkgbuild)) i

   setCurrentDirectory cwd

   system $ "rm -rf " ++ dir </> "{pkg,src,*.tar.gz}"
   tarred <- myReadProcess "tar" ["-zcvvf",(dir <.> "tar.gz"), dir] []
   case tarred of
        Left (_,s,_)  -> do
            hPutStrLn stderr s
            die "Unable to tar package"
        Right _ -> putStrLn ("Created " ++ (cwd </> dir <.> "tar.gz"))

   -- If the user created a .cabal2arch.log file, append log results there.
   mh <- getEnvMaybe "HOME"
   case mh of
        Nothing   -> return ()
        Just home -> do
           b <- doesFileExist $ home </> ".cabal2arch.log"
           if not b
              then return ()
              else do

               -- Log to build file.
               appendFile (home </> ".cabal2arch.log") $ (show $ (,,)

                   (arch_pkgname pkgbuild ++ "-" ++ (render . disp $ arch_pkgver pkgbuild))
                   (arch_pkgdesc pkgbuild)
                   (arch_url pkgbuild)) ++ "\n"

------------------------------------------------------------------------

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
getMD5 _ = die "Malformed PkgBuild"

-- attempt to filter out core packages we've already satisified
-- not actuall correct, since it doesn't take any version
-- info into account.
--
-- TODO this should use configDependency to find the precise
-- versions we have available on Arch.
--
removeCoreFrom :: [Dependency] -> [Dependency]
removeCoreFrom []               = []
removeCoreFrom (x@(Dependency n vr):xs) =
  case find (\(Dependency k _) -> n == k) corePackages of
    -- haskell-parsec, haskell-quickcheck
    Just (Dependency _ (ThisVersion v'))
        | withinRange v' vr         ->     removeCoreFrom xs

    Just (Dependency (PackageName "base") _)
                                    ->     removeCoreFrom xs

    Just (Dependency _ AnyVersion)  ->     removeCoreFrom xs
    _                               -> x : removeCoreFrom xs

--
-- Core packages and their versions. These come with
-- ghc, so we should be right.
--
-- http://haskell.org/haskellwiki/Libraries_released_with_GHC
--
-- And what Arch Linux thinks GHC provides:
--
-- http://repos.archlinux.org/wsvn/packages/ghc/repos/extra-x86_64/PKGBUILD
--
-- Note: we could just list these directly, and have yaourt solve them.
--
-- NEW POLICY:
--      We rely on all "provides" from the GHC library to be listed explicitly.
--
corePackages :: [Dependency]
corePackages =
    [

-- Magic packages we have to remove
     Dependency (PackageName "base")             (ThisVersion (Version  [4,1,0,0] []))
    ,Dependency (PackageName "dph-base")           (AnyVersion)
    ,Dependency (PackageName "dph-par" )           (AnyVersion)
    ,Dependency (PackageName "dph-prim-interface") (AnyVersion)
    ,Dependency (PackageName "dph-prim-par"   )    (AnyVersion)
    ,Dependency (PackageName "dph-prim-seq"   )    (AnyVersion)
    ,Dependency (PackageName "dph-seq"        )    (AnyVersion)
    ,Dependency (PackageName "ghc")              (AnyVersion)
    ,Dependency (PackageName "ghc-prim")         (AnyVersion)
    ,Dependency (PackageName "integer")         (AnyVersion)
    ,Dependency (PackageName "integer-gmp")         (AnyVersion)
    ,Dependency (PackageName "ghc-binary")         (AnyVersion)

-- Official Provides: http://repos.archlinux.org/wsvn/packages/ghc/repos/extra-x86_64/PKGBUILD
--  ,Dependency (PackageName "array")            (ThisVersion (Version  [0,3,0,0] []))
--  ,Dependency (PackageName "bytestring")       (ThisVersion (Version  [0,9,1,5] []))
--  ,Dependency (PackageName "Cabal")            (ThisVersion (Version  [1,8,0,2] []))
--  ,Dependency (PackageName "containers")       (ThisVersion (Version  [0,3,0,0] []))
--  ,Dependency (PackageName "directory")        (ThisVersion (Version  [1,0,1,0] []))
--  ,Dependency (PackageName "extensible-exceptions")         (AnyVersion)
--  ,Dependency (PackageName "filepath")         (ThisVersion (Version  [1,1,0,3] []))
--  ,Dependency (PackageName "haskell98")        (ThisVersion (Version  [1,0,1,1] []))
--  ,Dependency (PackageName "hpc")              (ThisVersion (Version  [0,5,0,4] []))
--  ,Dependency (PackageName "old-locale")       (ThisVersion (Version  [1,0,0,2] []))
--  ,Dependency (PackageName "old-time")         (ThisVersion (Version  [1,0,0,1] []))
--  ,Dependency (PackageName "pretty")           (ThisVersion (Version  [1,0,1,1] []))
--  ,Dependency (PackageName "process")          (ThisVersion (Version  [1,0,1,2] []))
--  ,Dependency (PackageName "random")           (ThisVersion (Version  [1,0,0,2] []))
--  ,Dependency (PackageName "syb")              (ThisVersion (Version  [0,1,0,2] []))
--  ,Dependency (PackageName "template-haskell") (ThisVersion (Version  [2,4,0,0] []))
--  ,Dependency (PackageName "time")             (ThisVersion (Version  [1,1,4] []))
--  ,Dependency (PackageName "unix")             (ThisVersion (Version  [2,4,0,0] []))
--  utf8-string


-- Removed in 6.12.x
--  ,Dependency (PackageName "html")             (ThisVersion (Version  [1,0,1,2] []))
--  ,Dependency (PackageName "integer")          (ThisVersion (Version  [0,1,0,0] []))
--  ,Dependency (PackageName "QuickCheck")       (ThisVersion (Version  [1,2,0,0] []))
--  ,Dependency (PackageName "haskell-src")      (ThisVersion (Version  [1,0,1,3] []))
--  ,Dependency (PackageName "parsec")           (ThisVersion (Version  [2,1,0,0] []))
--  ,Dependency (PackageName "packedstring")     (ThisVersion (Version  [0,1,0,1] []))
--  ,Dependency (PackageName "parallel")         (ThisVersion (Version  [1,1,0,0] []))
--  ,Dependency (PackageName "network")          (ThisVersion (Version  [2,2,0,1] []))
--  ,Dependency (PackageName "mtl")              (ThisVersion (Version  [1,1,0,2] []))
--  ,Dependency (PackageName "stm")              (ThisVersion (Version  [2,1,1,2] []))
--  ,Dependency (PackageName "HUnit")            (ThisVersion (Version  [1,2,0,3] []))
--  ,Dependency (PackageName "xhtml")            (ThisVersion (Version  [3000,2,0,1] []))
--  ,Dependency (PackageName "regex-base")       (ThisVersion (Version  [0,72,0,2] []))
--  ,Dependency (PackageName "regex-compat")     (ThisVersion (Version  [0,71,0,1] []))
--  ,Dependency (PackageName "regex-posix")      (ThisVersion (Version  [0,72,0,2] []))

-- Removed in 6.10.x
--  ,Dependency (PackageName "editline")         (AnyVersion)
--   Dependency (PackageName "ALUT")             (ThisVersion (Version  [2,1,0,0] []))
--  ,Dependency (PackageName "cgi")              (ThisVersion (Version  [3001,1,5,1] []))
--  ,Dependency (PackageName "fgl")              (ThisVersion (Version  [5,4,1,1] [])) -- gone
--  ,Dependency (PackageName "GLUT")             (ThisVersion (Version  [2,1,1,1] []))
--  ,Dependency (PackageName "OpenAL")           (ThisVersion (Version  [1,3,1,1] [])) -- gone
--  ,Dependency (PackageName "readline")         (ThisVersion (Version  [1,0,1,0] []))

    ]

-- Return the path to a .cabal file.
-- If not arguments are specified, use ".",
-- if the argument looks like a url, download that
-- otherwise, assume its a directory
--
findCabalFile :: FilePath -> FilePath -> IO FilePath
findCabalFile cwd tmp = do
   args <- getArgs
   let epath | null args
                = Right cwd
             | "http://" `isPrefixOf` file
                = Left file
             | ".cabal"  `isSuffixOf` file
                = Right (makeValid (joinPath [cwd,file]))
             | otherwise  -- a directory path
                = Right file

         where file = head args

   -- download url to .cabal
   case epath of
       Left url -> do
        eres <- myReadProcess "wget" [url] []
        case eres of
           Left (_,s,_) -> do
                hPutStrLn stderr s
                die $ "Couldn't download .cabal file: " ++ show url
           Right _ ->
                findPackageDesc tmp -- tmp dir

   -- it might be a .cabal file
       Right f | ".cabal" `isSuffixOf` f -> do
         b <- doesFileExist f
         if not b
            then die $ ".cabal file doesn't exist: " ++ show f
            else return f

   -- or assume it is a dir to a file:
       Right dir -> do
         b <- doesDirectoryExist dir
         if not b
            then die $ "directory doesn't exist: " ++ show dir
            else findPackageDesc dir

------------------------------------------------------------------------
-- Some extras
--

help :: IO a
help = do
 hPutStrLn stderr $ unlines
    [ "cabal2arch: [-h|--help] [directory|url]"
    , ""
    , "  Generate PKGBUILD for the .cabal file in <directory> or at <url>"
    , ""
    , "Usage:"
    , "   -h    Display help message"
    , ""
    , "Arguments: <directory>"
    , "              Look for .cabal file in <directory>"
    , "              If directory is empty, use pwd"
    , "           <file.cabal>"
    , "              Use .cabal file as source"
    , "           <url>"
    , "              Download .cabal file from <url>"
    ]
 exitWith ExitSuccess

------------------------------------------------------------------------

die :: String -> IO a
die s = do
    hPutStrLn stderr $ "cabal2pkg:\n" ++ s
    exitWith (ExitFailure 1)

(<->) :: String -> String -> String
x <-> y = x ++ "-" ++ y

-- Safe wrapper for getEnv
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe name = C.handle (const $ return Nothing) (Just `fmap` getEnv name)

------------------------------------------------------------------------

--
-- Strict process reading
--
myReadProcess :: FilePath                              -- ^ command to run
            -> [String]                              -- ^ any arguments
            -> String                                -- ^ standard input
            -> IO (Either (ExitCode,String,String) String)  -- ^ either the stdout, or an exitcode and any output

myReadProcess cmd args input = C.handle (return . handler) $ do
    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ (C.evaluate (length output) >> putMVar outMVar ())

    errput  <- hGetContents errh
    errMVar <- newEmptyMVar
    forkIO $ (C.evaluate (length errput) >> putMVar errMVar ())

    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    takeMVar errMVar
    ex     <- C.catch (waitForProcess pid) (\_e -> return ExitSuccess)
    hClose outh
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left (ex, errput, output)

  where
    handler (C.ExitException e) = Left (e,"","")
    handler e                   = Left (ExitFailure 1, show e, "")

