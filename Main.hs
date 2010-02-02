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
-- C libraries are dynamicall linked, should be listed in depends,
-- rather than makedepends

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Utils hiding (die)
import Distribution.Verbosity
import Distribution.Version
import Distribution.Package
import Distribution.License
import Distribution.Text
import Distribution.Compiler
import Distribution.System
import Distribution.Simple.PackageIndex

-- from the archlinux package:
import Distribution.ArchLinux.PkgBuild

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Concurrent
import qualified Control.OldException as C

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Data.Char
import Debug.Trace

import Text.PrettyPrint

import Paths_cabal2arch

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process


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
   let doc       = pkg2doc email pkgbuild

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

-- | Given an abstract pkgbuild, download the source bundle,
-- and compute its md5, returning a modified PkgBuild with
-- the md5 set.
--
-- TODO we may want to use a local package.
--
getMD5 :: PkgBuild -> IO PkgBuild
getMD5 pkg@(PkgBuild { arch_source = ArchList [url] }) = do
   hPutStrLn stderr $ "Fetching " ++ url
   hFlush stderr
   eres <- myReadProcess "wget" [url] []
   case eres of
       Left (_,s,_) -> do
            hPutStrLn stderr s
            hPutStrLn stderr $ "Couldn't download package: " ++ show url
            return pkg
       Right _ -> do
            src <- B.readFile (takeBaseName url <.> "gz")
            let !md5sum = show (md5 src)
            return pkg { arch_md5sum = ArchList [md5sum] }
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
    ,Dependency (PackageName "dph-base")           (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "dph-par" )           (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "dph-prim-interface") (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "dph-prim-par"   )    (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "dph-prim-seq"   )    (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "dph-seq"        )    (ThisVersion (Version [ 0,3 ] [] ))
    ,Dependency (PackageName "ghc")              (AnyVersion)
    ,Dependency (PackageName "ghc-prim")         (AnyVersion)
    ,Dependency (PackageName "integer")         (AnyVersion)
    ,Dependency (PackageName "integer-gmp")         (AnyVersion)

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

findCLibs :: PackageDescription -> [String]
findCLibs (PackageDescription { library = lib, executables = exe }) =
    -- warn for packages not in list.
    map (canonicalise . map toLower) (some ++ rest)
  where
    some = concatMap (extraLibs.buildInfo) exe
    rest = case lib of
                    Nothing -> []
                    Just l  -> extraLibs (libBuildInfo l)

    canonicalise k = case M.lookup k translationTable of
        Nothing -> trace ("WARNING: this library depends on a C library we do not know the pacman name for (" ++ map toLower k ++ ") . Check the C library names in the generated PKGBUILD File") $ map toLower k
        Just s  -> s

    -- known pacman packages for C libraries we use:
    translationTable = M.fromList
        [("Imlib2",     "imlib2")
        ,("SDL",        "sdl")
        ,("alut",       "freealut")
        ,("bz2",        "bzip2")
        ,("cblas",      "blas")
        ,("crack",      "cracklib")
        ,("crypto",     "openssl")
        ,("curl",       "curl")
        ,("freetype",   "freetype2")
        ,("glib",       "glib2")
        ,("wmflite",    "libwmf")
        ,("il",    "devil")

        ,("jpeg",       "libjpeg")
        ,("ldap",       "libldap")
        ,("pcap",       "libpcap")
        ,("png",        "libpng")
        ,("x11",        "libx11")
        ,("xrandr",     "libxrandr")
        ,("xml2",       "libxml2")
        ,("exif",       "libexif")
        ,("tiff",       "libtiff")
        ,("sndfile",    "libsndfile")
        ,("gcrypt",     "libgcrypt")
        ,("fftw3",      "fftw")

        ,("pq",         "postgresql")
        ,("ssl",        "openssl")
        ,("wx",         "wxgtk")
        ,("xenctrl",    "xen")
        ,("odbc",       "unixodbc")
        ,("z",          "zlib")
        ,("curses",     "ncurses")
        ,("xslt",       "libxslt")
        ,("csound64",   "csound5")
        ,("uuid",       "e2fsprogs")
        ,("doublefann", "fann")
        ,("ev",         "libev")

        ,("pthread",    "")
        ,("m",          "")
        ,("gl",         "")
        ,("glu",        "")
        ,("db_cxx",     "")
        ,("db_cxx",     "")
        ,("xdamage",    "")

        ,("icui18n",          "icu")
        ,("icuuc",          "icu")
        ,("icudata",          "icu")

        ,("netsnmp",        "net-snmp")
        ,("asound",        "alsa-lib")
        ,("ffi",        "libffi")
        ,("ogg",        "libogg")
        ,("theora",        "libtheora")
        ]
        -- atlas

shouldNotBeLibraries :: [String]
shouldNotBeLibraries =
    ["xmonad"
    ,"gitit"
    ,"l-seed"
    ,"hspresent"
    ,"haskell-platform"
    ,"xmonad-contrib"
    ,"lambdabot"
    ,"piet"
    ,"hsffig"
    ,"yi"
    ,"haddock"
    ,"hscolour"
    ,"line2pdf"
    ,"distract"
    ,"derive"
    ,"Hedi"
    ,"conjure"
    ,"clevercss"
    ,"cpphs"
    ,"backdropper"
    ,"darcs-beta"
    ,"gtk2hs"
    ,"darcs"
    ,"greencard"
-- the pandoc package doesnt' ship haskell-pandoc
--    ,"pandoc"
    ,"pugs-drift"
    ,"wol"
    ,"timepiece"
    ,"hledger"
    ,"hp2any-graph"
    ,"hp2any-manager"
    ]

-- translate some library dependencies to gtk names
--
gtk2hsIfy :: [Dependency] -> [Dependency]
gtk2hsIfy [] = []
gtk2hsIfy xs | foundSome = Dependency (PackageName "gtk2hs") AnyVersion :
                           [ v | v@(Dependency n _) <- xs
                           , n `notElem` gtkLibs ]
             | otherwise = xs

    where
        foundSome = not . null $ filter (`elem` gtkLibs) (map unDep xs)
        unDep (Dependency n _) = n


gtkLibs :: [PackageName]
gtkLibs = map PackageName
    ["glade" -- guihaskell
    ,"cairo"
    ,"glib"
    ,"gtk"
    ,"gconf"
    ,"gtkglext"
    ,"gtksourceview2"
    ,"mozembed"
    ,"svgcairo"
    ]

------------------------------------------------------------------------
-- Parsing and pretty printing:

--
-- | Translate an abstract PkgBuild file into a document structure
--
pkg2doc :: String -> PkgBuild -> Doc
pkg2doc email pkg = vcat
 [ text "# Contributor:"
    <+> text email
 , text "# Package generated by cabal2arch" <+> disp version
 , text "# Note: we list all package dependencies."
 , text "# Your package tool should understand 'provides' syntax"
 , text "#"
 , text "# Keep up to date on http://archhaskell.wordpress.com/"
 , text "#"
 , text "pkgname"
    <=> text (arch_pkgname pkg)
 , text "pkgrel"
    <=> int (arch_pkgrel pkg)
 , text "pkgver"
    <=> disp (arch_pkgver pkg)
 , text "pkgdesc"
    <=> text (show (arch_pkgdesc pkg))
 , text "url"
    <=> doubleQuotes (text (arch_url pkg))
 , text "license"
    <=> disp (arch_license pkg)
 , text "arch"
    <=> disp (arch_arch pkg)
 , text "makedepends"
    <=> disp (arch_makedepends pkg)
 , case arch_depends pkg of
        ArchList [] -> empty
        _           -> text "depends" <=> disp (arch_depends pkg)
 , text "options" <=> disp (arch_options pkg)
 , text "source"
    <=> dispNoQuotes (arch_source pkg)
 , case arch_install pkg of
    Nothing -> empty
    Just p  -> text "install" <=> disp p
 , text "md5sums"
    <=> disp (arch_md5sum pkg)
 , hang
    (text "build() {") 4
             (vcat $ (map text) (arch_build pkg))
   $$ char '}'
 ]

--
-- | Tranlsate a generic cabal file into a PGKBUILD
--
cabal2pkg :: PackageDescription -> (PkgBuild, Maybe String)
cabal2pkg cabal

-- TODO decide if its a library or an executable,
-- handle mullltipackages
-- extract C dependencies

-- = trace (show cabal) $
  =
  (emptyPkgBuild
    { arch_pkgname = archName
    , arch_pkgver  = vers
    , arch_url     = "http://hackage.haskell.org/package/"++display name
 --       else homepage cabal
    , arch_pkgdesc = case synopsis cabal of
                          [] -> take 80 (description cabal)
                          s  -> s
    , arch_license =
        ArchList . return $
            case license cabal of
                x@GPL {} -> x
                x@LGPL {} -> x
                l    -> UnknownLicense ("custom:"++ show l)

    -- All Hackage packages depend on GHC at build time
    -- All Haskell libraries are prefixed with "haskell-"
    , arch_makedepends = if not hasLibrary
            then my_makedepends
            else ArchList [] -- makedepends should not duplicate depends

    , arch_depends =
        (if not (isLibrary)
            then
                ArchList [ArchDep (Dependency (PackageName "gmp") AnyVersion)]
                                `mappend`
                                anyClibraries
            else ArchList [])
        `mappend`
            -- libraries have 'register-time' dependencies on
            -- their dependent Haskell libraries.
            --
           (if hasLibrary then my_makedepends
                          else ArchList [])

    -- need the dependencies of all flags that are on by default, for all libraries and executables

    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/"
       ++ (display name </> display vers </> display name <-> display vers <.> "tar.gz")

    , arch_build =
        [ "cd ${srcdir}/" </> display name <-> display vers
        , "runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} || return 1"
        , "runhaskell Setup build                   || return 1"
        ] ++

    -- Only needed for libraries:
        (if hasLibrary
           then
            [ "runhaskell Setup haddock || return 1"
            , "runhaskell Setup register   --gen-script || return 1"
            , "runhaskell Setup unregister --gen-script || return 1"
            , "install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/$pkgname/register.sh"
            , "install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/$pkgname/unregister.sh"
            , "install -d -m755 $pkgdir/usr/share/doc/ghc/html/libraries"
            , "ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/html/libraries/" ++ (display name)
            ]
           else [])
         ++
         ["runhaskell Setup copy --destdir=${pkgdir} || return 1"]
         ++
         (if not (null (licenseFile cabal)) && (case license cabal of GPL {} -> False; LGPL {} -> False; _ -> True)
          then
              [ "install -D -m644 " ++ licenseFile cabal ++ " ${pkgdir}/usr/share/licenses/$pkgname/LICENSE || return 1"
              , "rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
              ]
          else [])

    -- if its a library:
    , arch_install = if hasLibrary then Just $ install_hook_name archName
                                   else Nothing

    }, if hasLibrary
          then Just (install_hook archName)
          else Nothing
    )

  where
    archName = map toLower (if isLibrary then "haskell-" ++ display name else display name)
    name     = pkgName (package cabal)
    vers     = pkgVersion (package cabal)

    -- build time dependencies
    my_makedepends =
     (arch_makedepends emptyPkgBuild)
        `mappend`
     -- Haskell libraries
     -- TODO: use a real package spec to compute these names
     -- based on what is in Arch.
     ArchList
         [ ArchDep (Dependency (PackageName $
               if d `notElem` shouldNotBeLibraries
                    then "haskell" <-> map toLower (display d) else display d) v)
         | Dependency (PackageName d) v <- gtk2hsIfy (buildDepends cabal) ]
        `mappend`
     anyClibraries
        `mappend`
     ArchList [ ArchDep d | b <- allBuildInfo cabal
                          , d@(Dependency n _) <- buildTools b
                          , n /= PackageName "hsc2hs"  ]

    -- TODO: need a 'nub' in here.

    hasLibrary = isJust (library cabal)
    isLibrary  = isJust (library cabal) -- && null (executables cabal)
                    && map toLower (display name) `notElem` shouldNotBeLibraries

    anyClibraries | null libs = ArchList []
                  | otherwise = ArchList libs
       where
         libs = [ ArchDep (Dependency (PackageName s) AnyVersion) | s <- nub (findCLibs cabal) ]

-- quickcheck 2.
-- parsec 3

--
-- post install, and pre-remove hooks to run, to sync up ghc-pkg
--
install_hook_name :: String -> String
install_hook_name pkgname = pkgname <.> "install"

install_hook :: String -> String
install_hook pkgname = unlines
    [ "HS_DIR=/usr/share/haskell/" ++ pkgname
    , "post_install() {"
    , "  ${HS_DIR}/register.sh"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "pre_upgrade() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "post_upgrade() {"
    , "  ${HS_DIR}/register.sh"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "pre_remove() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "post_remove() {"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "op=$1"
    , "shift"
    , "$op $*" ]

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

(<=>) :: Doc -> Doc -> Doc
x <=> y = x <> char '=' <> y

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

