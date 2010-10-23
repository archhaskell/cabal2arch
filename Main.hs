{-# LANGUAGE DeriveDataTypeable #-}

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

import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils hiding (die)
import Distribution.Verbosity
import Distribution.Text

-- from the archlinux package:
import Distribution.ArchLinux.PkgBuild
import Distribution.ArchLinux.CabalTranslation
import Distribution.ArchLinux.SystemProvides

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as CE

import Data.List

import Text.PrettyPrint

import Paths_cabal2arch

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process hiding(cwd)

import System.Console.CmdArgs

data CmdLnArgs
    = CmdLnConvertOne { argCabalFile :: String }
    | CmdLnConvertMany { argPkgList :: FilePath, argTarBall :: FilePath, argRepo :: FilePath }
    deriving (Data, Typeable)

cmdLnConvertOne :: CmdLnArgs
cmdLnConvertOne = CmdLnConvertOne { argCabalFile = "" &= argPos 0 &= typ "FILE|DIR|URL" }
    &= auto &= name "conv" &= help "Convert a single CABAL file."

cmdLnConvertMany :: CmdLnArgs
cmdLnConvertMany = CmdLnConvertMany
    { argPkgList = def &= argPos 0 &= typFile
    , argTarBall = def &= argPos 1 &= typFile
    , argRepo = def &= argPos 2 &= typDir
    } &= name "convtar" &= help "Convert a tarball of CABAL files into an ABS tree."
    &= details
        [ "  cabal2arch convtar list tar abs"
        , "'list' is a file consisting of lines of the form \"<pkg name> <version>\"."
        , "'tar' is a tar ball of package descriptions (CABAL files) like the one published on Hackage:", "  http://hackage.haskell.org/packages/archive/00-index.tar.gz"
        , "'abs' is a directory where the ABS tree will be created."
        ]

cmdLnArgs :: CmdLnArgs
cmdLnArgs = modes [cmdLnConvertOne, cmdLnConvertMany]
    &= program "cabal2arch"
    &= summary "cabal2arch: Convert .cabal file to ArchLinux source package"

main :: IO ()
main =
    CE.bracket
        -- We do all our work in a temp directory
        (do _cwd  <- getCurrentDirectory
            etmp <- myReadProcess "mktemp" ["-d"] []
            case etmp of
                Left _  -> die "Unable to create temp directory"
                Right d -> do
                    let dir = makeValid (init d) -- drop newline
                    setCurrentDirectory dir
                    return (dir, _cwd))

        -- Always remember to clean up
        (\(d, _cwd) -> do
            setCurrentDirectory _cwd
            removeDirectoryRecursive d)

        -- Now, get to work:
        $ \(tmp, _cwd) -> do

            myArgs <- cmdArgs cmdLnArgs
            email <- do
                r <- getEnvMaybe "ARCH_HASKELL"
                case r of
                    Nothing -> do
                        hPutStrLn stderr "Warning: ARCH_HASKELL environment variable not set. Set this to the maintainer contact you wish to use. \n E.g. 'Arch Haskell Team <arch-haskell@haskell.org>'"
                        return []
                    Just s  -> return s

            cabalfile <- findCabalFile myArgs _cwd tmp
            hPutStrLn stderr $ "Using " ++ cabalfile

            cabalsrc  <- readPackageDescription normal cabalfile

            -- Create a package description with all configurations resolved.
            sysProvides <- getDefaultSystemProvides
            let finalcabal = preprocessCabal cabalsrc sysProvides
            finalcabal' <- case finalcabal of
                Nothing -> die "Aborting..."
                Just f -> return f
            let (pkgbuild', hooks) = cabal2pkg finalcabal' sysProvides

            apkgbuild' <- getMD5 pkgbuild'
            let apkgbuild = apkgbuild' { pkgBuiltWith = Just version }
                pkgbuild = pkgBody apkgbuild
                doc = pkg2doc email apkgbuild
                dir = arch_pkgname pkgbuild

            setCurrentDirectory _cwd
            createDirectoryIfMissing False dir
            setCurrentDirectory dir

            writeFile "PKGBUILD" (render doc ++ "\n")

            -- print pkgname.install
            case hooks of
                Nothing -> return ()
                Just i  -> writeFile (install_hook_name (arch_pkgname pkgbuild)) i

            setCurrentDirectory _cwd

            _ <- system $ "rm -rf " ++ dir </> "{pkg,src,*.tar.gz}"
            tarred <- myReadProcess "tar" ["-zcvvf",(dir <.> "tar.gz"), dir] []
            case tarred of
                Left (_,s,_)  -> do
                    hPutStrLn stderr s
                    die "Unable to tar package"
                Right _ -> putStrLn ("Created " ++ (_cwd </> dir <.> "tar.gz"))

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
                                (arch_pkgname pkgbuild ++ "-" ++ (display $ arch_pkgver pkgbuild))
                                (arch_pkgdesc pkgbuild)
                                (arch_url pkgbuild)) ++ "\n"

------------------------------------------------------------------------

-- | Given an abstract pkgbuild, run "makepkg -g" to compute md5
-- of source files (possibly cached locally), and modify the PkgBuild
-- accordingly.
--
getMD5 :: AnnotatedPkgBuild -> IO AnnotatedPkgBuild
getMD5 pkg = do
    putStrLn "Feeding the PKGBUILD to `makepkg -g`..."
    eres <- readProcessWithExitCode "makepkg" ["-g"] $ display pkg
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
                    in return pkg { pkgBody = (pkgBody pkg) { arch_md5sum = ArchList [md5sum] } }
                else do
                    hPutStrLn stderr $ "Incorrect output from makepkg."
                    return pkg

-- Return the path to a .cabal file.
-- If not arguments are specified, use ".",
-- if the argument looks like a url, download that
-- otherwise, assume its a directory
--
findCabalFile :: CmdLnArgs -> FilePath -> FilePath -> IO FilePath
findCabalFile (CmdLnConvertMany {}) _ _ = error "TBD!!!"
findCabalFile _args@(CmdLnConvertOne {}) _cwd tmp = do
    let epath
            | null file
                = Right _cwd
            | "http://" `isPrefixOf` file
                = Left file
            | ".cabal"  `isSuffixOf` file
                = Right (makeValid (joinPath [_cwd,file]))
            | otherwise  -- a directory path
                = Right file where file = argCabalFile _args

    -- download url to .cabal
    case epath of
        Left url -> do
            eres <- myReadProcess "wget" [url] []
            case eres of
                Left (_,s,_) -> do
                    hPutStrLn stderr s
                    die $ "Couldn't download .cabal file: " ++ show url
                Right _ -> findPackageDesc tmp -- tmp dir

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

die :: String -> IO a
die s = do
    hPutStrLn stderr $ "cabal2pkg:\n" ++ s
    exitWith (ExitFailure 1)

-- Safe wrapper for getEnv
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe _name = CE.handle ((const :: a -> CE.SomeException -> a) $ return Nothing) (Just `fmap` getEnv _name)

------------------------------------------------------------------------

--
-- Strict process reading
--
myReadProcess :: FilePath                              -- ^ command to run
            -> [String]                              -- ^ any arguments
            -> String                                -- ^ standard input
            -> IO (Either (ExitCode,String,String) String)  -- ^ either the stdout, or an exitcode and any output

myReadProcess cmd _args input = CE.handle (return . handler) $ do
    (inh,outh,errh,pid) <- runInteractiveProcess cmd _args Nothing Nothing

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ (CE.evaluate (length output) >> putMVar outMVar ())

    errput  <- hGetContents errh
    errMVar <- newEmptyMVar
    _ <- forkIO $ (CE.evaluate (length errput) >> putMVar errMVar ())

    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    takeMVar errMVar
    ex <- CE.catch (waitForProcess pid) ((const :: a -> CE.SomeException -> a) $ return ExitSuccess)
    hClose outh
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left (ex, errput, output)

    where
        handler (ExitFailure e) = Left (ExitFailure e,"","")
        handler e               = Left (ExitFailure 1, show e, "")
