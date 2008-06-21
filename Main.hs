{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE BangPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- |
-- Module    : cabal2arch: convert cabal packages to Arch Linux PKGBUILD format
-- Copyright : (c) Don Stewart, 2008
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

import Distribution.PackageDescription
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

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Concurrent
import Control.Exception
import qualified Control.Exception as C

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
 bracket
   -- We do all our work in a temp directory
  (do cwd  <- getCurrentDirectory
      etmp <- readProcess "mktemp" ["-d"] []
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
            Nothing -> do hPutStrLn stderr "Warning: ARCH_HASKELL environment variable not set. Set this to the maintainer contat you wish to use. \n E.g. 'Arch Haskell Team <arch-haskell@haskell.org>'"
                          return []
            Just s  -> return s

   cabalfile <- findCabalFile cwd tmp
   hPutStrLn stderr $ "Using " ++ cabalfile

   cabalsrc  <- readPackageDescription normal cabalfile

   -- Create a package description with all configurations resolved.
   let e_finalcabalsrc = finalizePackageDescription
        []
        (Nothing :: Maybe (PackageIndex PackageIdentifier))
        buildOS -- linux/x86_64
        X86_64
        (CompilerId GHC (Version [6,8,2] []))

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

   tarred <- readProcess "tar" ["-zcvvf",(dir <.> "tar.gz"), dir] []
   case tarred of
        Left (_,s,_)  -> do
            hPutStrLn stderr s
            die "Unable to tar package"
        Right _ -> putStrLn ("Created " ++ (cwd </> dir <.> "tar.gz"))

   -- RSS generation help
   writeFile "title" (arch_pkgname pkgbuild ++ "-" ++ (render . disp $ arch_pkgver pkgbuild))
   writeFile "desc"  (show $ arch_pkgdesc pkgbuild)

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
   eres <- readProcess "wget" [url] []
   case eres of
       Left (_,s,_) -> do
            hPutStrLn stderr s
            die $ "Couldn't download package: " ++ show url
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
removeCoreFrom (x@(Dependency n _):xs) =
  case find (\(Dependency k _) -> n == k) corePackages of
    Just _ -> removeCoreFrom xs
    Nothing -> x : removeCoreFrom xs

--
-- Core packages and their versions. These come with
-- ghc, so we should be right.
--
-- TODO populate this based on a dynamic check.
--
corePackages :: [Dependency]
corePackages =
    [Dependency "ALUT"             (ThisVersion (Version  [2,1,0,0] []))
    ,Dependency "Cabal"            (ThisVersion (Version  [1,2,3,0] []))
    ,Dependency "GLUT"             (ThisVersion (Version  [2,1,1,1] []))
    ,Dependency "HUnit"            (ThisVersion (Version  [1,2,0,0] []))
    ,Dependency "OpenAL"           (ThisVersion (Version  [1,3,1,1] []))
    ,Dependency "QuickCheck"       (ThisVersion (Version  [1,1,0,0] []))
    ,Dependency "array"            (ThisVersion (Version  [0,1,0,0] []))
    ,Dependency "base"             (ThisVersion (Version  [3,0,1,0] []))
    ,Dependency "bytestring"       (ThisVersion (Version  [0,9,0,1] []))
    ,Dependency "cgi"              (ThisVersion (Version  [3001,1,5,1] []))
    ,Dependency "ghc"              (AnyVersion)
    ,Dependency "containers"       (ThisVersion (Version  [0,1,0,1] []))
    ,Dependency "directory"        (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "fgl"              (ThisVersion (Version  [5,4,1,1] []))
    ,Dependency "filepath"         (ThisVersion (Version  [1,1,0,0] []))
    ,Dependency "haskell-src"      (ThisVersion (Version  [1,0,1,1] []))
    ,Dependency "haskell98"        (ThisVersion (Version  [1,0,1,0] []))
    ,Dependency "hpc"              (ThisVersion (Version  [0,5,0,0] []))
    ,Dependency "html"             (ThisVersion (Version  [1,0,1,1] []))
    ,Dependency "mtl"              (ThisVersion (Version  [1,1,0,1] []))
    ,Dependency "network"          (ThisVersion (Version  [2,1,0,0] []))
    ,Dependency "old-locale"       (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "old-time"         (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "packedstring"     (ThisVersion (Version  [0,1,0,0] []))
    ,Dependency "parallel"         (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "parsec"           (ThisVersion (Version  [2,1,0,0] []))
    ,Dependency "pretty"           (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "process"          (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "random"           (ThisVersion (Version  [1,0,0,0] []))
    ,Dependency "readline"         (ThisVersion (Version  [1,0,1,0] []))
    ,Dependency "regex-base"       (ThisVersion (Version  [0,72,0,1] []))
    ,Dependency "regex-compat"     (ThisVersion (Version  [0,71,0,1] []))
    ,Dependency "regex-posix"      (ThisVersion (Version  [0,72,0,2] []))
    ,Dependency "stm"              (ThisVersion (Version  [2,1,1,0] []))
    ,Dependency "template-haskell" (ThisVersion (Version  [2,2,0,0] []))
    ,Dependency "time"             (ThisVersion (Version  [1,1,2,0] []))
    ,Dependency "unix"             (ThisVersion (Version  [2,3,0,0] []))
    ,Dependency "xtml"             (ThisVersion (Version  [3000,0,2,1] []))

{-
ALUT-2.1.0.0        cgi-3001.1.5.1       network-2.1.0.0       regex-base-0.72.0.1
Cabal-1.2.3.0       containers-0.1.0.1   old-locale-1.0.0.0    regex-compat-0.71.0.1
GLUT-2.1.1.1        directory-1.0.0.0    old-time-1.0.0.0      regex-posix-0.72.0.2
HUnit-1.2.0.0       fgl-5.4.1.1          packedstring-0.1.0.0  stm-2.1.1.0
OpenAL-1.3.1.1      filepath-1.1.0.0     parallel-1.0.0.0      template-haskell-2.2.0.0
OpenGL-2.2.1.1      haskell-src-1.0.1.1  parsec-2.1.0.0        time-1.1.2.0
QuickCheck-1.1.0.0  haskell98-1.0.1.0    pretty-1.0.0.0        unix-2.3.0.0
array-0.1.0.0       hpc-0.5.0.0          process-1.0.0.0       xhtml-3000.0.2.1
base-3.0.1.0        html-1.0.1.1         random-1.0.0.0
bytestring-0.9.0.1  mtl-1.1.0.0          readline-1.0.1.0
-}

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
        eres <- readProcess "wget" [url] []
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
        ,("fftw3",      "fftw")

        ,("pq",         "postgresql")
        ,("ssl",        "openssl")
        ,("wx",         "wxgtk")
        ,("xenctrl",    "xen")
        ,("odbc",       "unixodbc")
        ,("z",          "zlib")
        ,("curses",     "ncurses")
        ,("xslt",       "libxslt")

        ,("pthread",     "")
        ,("m",          "")
        ]
        -- atlas

shouldNotBeLibraries :: [String]
shouldNotBeLibraries =
    ["xmonad"
    ,"hscolour"
    ,"distract"
    ,"Hedi"
    ,"conjure"
    ,"cpphs"
    ,"backdropper"
    ]

-- translate some library dependencies to gtk names
--
gtk2hsIfy :: [Dependency] -> [Dependency]
gtk2hsIfy [] = []
gtk2hsIfy xs | foundSome = Dependency "gtk2hs" AnyVersion :
                           [ v | v@(Dependency n _) <- xs
                           , n `notElem` gtkLibs ]
             | otherwise = xs

    where
        foundSome = not . null $ filter (`elem` gtkLibs) (map unDep xs)
        unDep (Dependency n _) = n


gtkLibs :: [String]
gtkLibs =
    ["glade" -- guihaskell
    ,"cairo"
    ,"glib"
    ,"gtk"
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
 , text "depends"
    <=> disp (arch_depends pkg)
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
    , arch_url     = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/"++name
 --       else homepage cabal
    , arch_pkgdesc = synopsis cabal
    , arch_license =
        ArchList . return $
            case license cabal of
                GPL  -> GPL
                LGPL -> LGPL
                l    -> UnknownLicense ("custom:"++ show l)

    -- All Hackage packages depend on GHC at build time
    -- All Haskell libraries are prefixed with "haskell-"
    , arch_makedepends = (arch_makedepends emptyPkgBuild)
                            `mappend`
                         -- Haskell libraries
                         -- TODO: use a real package spec to compute these names
                         -- based on what is in Arch.
                         ArchList
                             [ ArchDep (Dependency (
                                   if d /= "gtk2hs" then "haskell" <-> map toLower d else d) v)
                             | Dependency d v <- gtk2hsIfy (buildDepends cabal) ]
                            `mappend`
                         anyClibraries

    , arch_depends = ArchList [ArchDep (Dependency "gmp" AnyVersion)]
                    `mappend`
                    anyClibraries

    -- need the dependencies of all flags that are on by default, for all libraries and executables

    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/"
       ++ (name </> display vers </> name <-> display vers <.> "tar.gz")

    , arch_build =
        [ "cd $startdir/src/" </> name <-> display vers
        , "runhaskell Setup configure --enable-executable-stripping --prefix=/usr || return 1"
        , "runhaskell Setup build                   || return 1"
        ] ++

    -- Only needed for libraries:
        (if hasLibrary
           then
            ["runhaskell Setup register   --gen-script || return 1"
            ,"runhaskell Setup unregister --gen-script || return 1"
            ,"install -D -m744 register.sh   $startdir/pkg/usr/share/haskell/$pkgname/register.sh"
            , "install    -m744 unregister.sh $startdir/pkg/usr/share/haskell/$pkgname/unregister.sh"
            ]
           else [])
         ++
         ["runhaskell Setup copy --destdir=$startdir/pkg || return 1"]
         ++
         (if not (null (licenseFile cabal)) && license cabal `notElem` [GPL,LGPL]
          then ["install -D -m644 " ++ licenseFile cabal ++
                    " $startdir/pkg/usr/share/licenses/$pkgname/LICENSE || return 1" ]
          else [])

    -- if its a library:
    , arch_install = if hasLibrary then Just $ install_hook_name archName
                                   else Nothing

    }, if hasLibrary
          then Just (install_hook archName)
          else Nothing
    )

  where
    archName = map toLower (if isLibrary then "haskell-" ++ name else name)
    name     = pkgName (package cabal)
    vers     = pkgVersion (package cabal)

    hasLibrary = isJust (library cabal)
    isLibrary  = isJust (library cabal) -- && null (executables cabal)
                    && map toLower name `notElem` shouldNotBeLibraries

    anyClibraries | null libs = ArchList []
                  | otherwise = ArchList libs
       where
         libs = [ ArchDep (Dependency s AnyVersion) | s <- nub (findCLibs cabal) ]

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
    , "}"
    , "pre_upgrade() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "post_upgrade() {"
    , "  ${HS_DIR}/register.sh"
    , "}"
    , "pre_remove() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "op=$1"
    , "shift"
    , "$op $*" ]

-- 
-- | A data type to represent PKGBUILD files
-- 
data PkgBuild =
  PkgBuild
    { arch_pkgname :: String
        -- ^
        -- The name of the package. This has be a unix-friendly name
        -- as it will be used in the package filename.
    , arch_pkgver  :: Version
        -- ^ The version of the software as released from the authorii
        --  (e.g. ´2.7.1´).
    , arch_pkgrel  :: !Int
        -- ^
        --  This is the release number specific to the Arch Linux
        -- release. This allows package maintainers to make updates to
        -- the package´s configure flags, for example. A pkgrel of 1
        -- is typically used for each upstream software release and is
        -- incremented for intermediate PKGBUILD updates.
    , arch_pkgdesc :: String
        -- ^
        -- This should be a brief description of the package and its
        -- functionality. Try to keep the description to one line of text.
    , arch_arch    :: ArchList ArchArch
        -- ^
        -- Defines on which architectures the given package is
        -- available (e.g. arch=(´i686´ ´x86_64´)).
    , arch_url     :: String
        -- ^
        -- This field contains a URL that is associated with the software
        -- being packaged. This is typically the project´s website.
    , arch_license :: ArchList License
        -- ^
        -- This field specifies the license(s) that apply to the package.
        -- Commonly-used licenses are found in /usr/share/licenses/common. If
        -- you see the package´s license there, simply reference it in the
        -- license field (e.g.  license=(´GPL´)). If the package provides a
        -- license not found in /usr/share/licenses/common, then you should
        -- include the license in the package itself and set
        -- license=(´custom´) or license=(´custom:LicenseName´). The license
        -- should be placed in $pkgdir/usr/share/licenses/$pkgname when
        -- building the package. If multiple licenses are applicable for a
        -- package, list all of them: license=(´GPL´ ´FDL´).
    , arch_makedepends :: ArchList ArchDep
        -- ^
        -- An array of packages that this package depends on to build, but are
        -- not needed at runtime. Packages in this list follow the same format
        -- as depends.

    , arch_depends     :: ArchList ArchDep
        -- ^
        -- An array of packages that this package depends on to run. Packages
        -- in this list should be surrounded with single quotes and contain at
        -- least the package name. Entries can also include a version
        -- requirement of the form name<>version, where <> is one of five
        -- comparisons: >= (greater than or equal to), <= (less than or equal
        -- to), = (equal to), > (greater than), or < (less than).
    , arch_source      :: ArchList String
        -- ^
        -- An array of source files required to build the package. Source
        -- files must either reside in the same directory as the PKGBUILD
        -- file, or be a fully-qualified URL that makepkg will use to download
        -- the file. In order to make the PKGBUILD as useful as possible, use
        -- the $pkgname and $pkgver variables if possible when specifying the
        -- download location. Any files that are compressed will automatically
        -- be extracted, unless found in the noextract array listed below.
    , arch_md5sum      :: ArchList String
        -- ^
        -- This array contains an MD5 hash for every source file specified in
        -- the source array (in the same order). makepkg will use this to
        -- verify source file integrity during subsequent builds. To easily
        -- generate md5sums, run “makepkg -g >> PKGBUILD”. If desired, move
        -- the md5sums line to an appropriate location.  NOTE: makepkg
        -- supports multiple integrity algorithms and their corresponding
        -- arrays (i.e. sha1sums for the SHA1 algorithm); however, official
        -- packages use only md5sums for the time being.
    , arch_build        :: [String]
        -- ^
        -- The build hook

    , arch_install      :: Maybe String
        -- ^
        -- Specifies a special install script that is to be included in the package. This
        -- file should reside in the same directory as the PKGBUILD, and will be copied
        -- into the package by makepkg. It does not need to be included in the source
        -- array (e.g.  install=pkgname.install).


    }
    deriving (Show, Eq)

--
-- | An empty PKGBUILD
--
emptyPkgBuild :: PkgBuild
emptyPkgBuild =
  PkgBuild
    { arch_pkgname     = pkgName (package e)
    , arch_pkgver      = pkgVersion (package e)
    , arch_pkgrel      = 1
    , arch_pkgdesc     = synopsis e
    , arch_arch        = ArchList [Arch_X86, Arch_X86_64]
    , arch_url         = homepage e
    , arch_license     = ArchList [license e]
    , arch_makedepends = ArchList [(ArchDep (Dependency "ghc" AnyVersion))]
        -- makedepends=('ghc>=6.6') ?
    , arch_depends     = ArchList []
    , arch_source      = ArchList []
    , arch_md5sum      = ArchList []
        -- sha1sums=('a08670e4c749850714205f425cb460ed5a0a56b2')
    , arch_build       = []
    , arch_install     = Nothing  -- executable
    }
  where
    e = emptyPackageDescription

------------------------------------------------------------------------
-- Extra pretty printer instances and types

newtype ArchDep = ArchDep Dependency
  deriving (Eq,Show)

-- the PKGBUILD version spec is less expressive than cabal, we can't
-- really handle unions or intersections well yet.

instance Text ArchDep where
  disp (ArchDep (Dependency name ver)) =
    text name <> mydisp ver
   where
     --  >= (greater than or equal to), <= (less than or
     --  equal to), = (equal to), > (greater than), or <
      mydisp AnyVersion           = empty

      mydisp (ThisVersion    v)   = text "=" <> disp v
      mydisp (LaterVersion   v)   = char '>' <> disp v
      mydisp (EarlierVersion v)   = char '<' <> disp v

      mydisp (UnionVersionRanges (ThisVersion  v1) (LaterVersion v2))
        | v1 == v2 = text ">=" <> disp v1
      mydisp (UnionVersionRanges (LaterVersion v2) (ThisVersion  v1))
        | v1 == v2 = text ">=" <> disp v1
      mydisp (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2))
        | v1 == v2 = text "<=" <> disp v1
      mydisp (UnionVersionRanges (EarlierVersion v2) (ThisVersion v1))
        | v1 == v2 = text "<=" <> disp v1

{-
      mydisp (UnionVersionRanges r1 r2)
        = disp r1 <+> text "||" <+> disp r2

      mydisp (IntersectVersionRanges r1 r2)
        = disp r1 <+> text "&&" <+> disp r2
-}

      mydisp x = trace ("WARNING: Can't handle this version format yet: " ++ show x ++ "\ncheck the dependencies by hand.")$ empty

  parse = undefined

--
-- | Valid linux platforms
--
data ArchArch = Arch_X86 | Arch_X86_64
    deriving (Show, Eq)

instance Text ArchArch where
    disp x = case x of
       Arch_X86      -> text "i686"
       Arch_X86_64   -> text "x86_64"
    parse = error "Text.parrse not defined for ArchList"

-- Lists with quotes
newtype ArchList a = ArchList [a]
  deriving (Show, Eq, Monoid, Functor)

instance Text String where
    disp s = text s
    parse = error "Text.parse not defined for String"

instance Text a => Text (ArchList a) where
    disp (ArchList xs) =
         parens (hcat
                (intersperse space
                    (map (quotes . disp) xs)))
    parse = error "Text.parse not defined for ArchList"

-- | Printing with no quotes
dispNoQuotes :: Text a => ArchList a -> Doc
dispNoQuotes (ArchList xs) =
         parens (hcat
                (intersperse space
                    (map disp xs)))

------------------------------------------------------------------------

------------------------------------------------------------------------
-- Some extras
--

help :: IO a
help = do
 hPutStrLn stderr $ unlines
    [ "cabal2pkg: [-h|--help] [directory|url]"
    , ""
    , "  Generate PKGBUILD to stdout for .cabal file in <directory> or at <url>"
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
getEnvMaybe name = handle (const $ return Nothing) (Just `fmap` getEnv name)

------------------------------------------------------------------------

--
-- Strict process reading 
--
readProcess :: FilePath                              -- ^ command to run
            -> [String]                              -- ^ any arguments
            -> String                                -- ^ standard input
            -> IO (Either (ExitCode,String,String) String)  -- ^ either the stdout, or an exitcode and any output

readProcess cmd args input = C.handle (return . handler) $ do
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

