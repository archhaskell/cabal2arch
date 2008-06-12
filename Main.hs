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

-- TODO: libraries should be "haskell-$packagefoo"

import Distribution.PackageDescription
import Distribution.Simple.Utils hiding (die)
import Distribution.Verbosity
import Distribution.Version
import Distribution.Package
import Distribution.License
import Distribution.Text

import Data.Digest.MD5
import qualified Data.ByteString.Lazy as B

import Control.Exception
import Data.List
import Data.Monoid
import System.Environment
import Control.Concurrent
import qualified Control.Exception as C
import System.Process
import Text.PrettyPrint
import System.FilePath
import System.Exit
import System.IO
import Control.Monad
import System.Directory
import Debug.Trace

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
   email     <- maybe "" id `fmap` getEnvMaybe "DARCS_EMAIL"

   cabalfile <- findCabalFile cwd tmp
   hPutStrLn stderr $ "Using " ++ cabalfile

   cabalsrc  <- readPackageDescription normal cabalfile
   let pkgbuild' = cabal2pkg cabalsrc

   pkgbuild  <- getMD5 pkgbuild'
   let doc       = pkg2doc email pkgbuild
   putStrLn (render doc)

------------------------------------------------------------------------

-- | Given an abstract pkgbuild, download the source bundle,
-- and compute its md5, returning a modified PkgBuild with
-- the md5 set.
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

------------------------------------------------------------------------
-- Parsing and pretty printing:

-- 
-- | Translate an abstract PkgBuild file into a document structure
-- 
pkg2doc :: String -> PkgBuild -> Doc
pkg2doc email pkg = vcat
 [ text "# Contributor:"
    <+> text email
 , text "pkgname"
    <=> text (arch_pkgname pkg)
 , text "pkgrel"
    <=> int (arch_pkgrel pkg)
 , text "pkgver"
    <=> disp (arch_pkgver pkg)
 , text "pkgdesc"
    <=> doubleQuotes (text (arch_pkgdesc pkg))
 , text "url"
    <=> doubleQuotes (text (arch_url pkg))
 , text "license"
    <=> disp (arch_license pkg)
 , text "arch"
    <=> disp (arch_arch pkg)
 , text "makedepends"
    <=> disp (arch_makedepends pkg)
 , text "source"
    <=> dispNoQuotes (arch_source pkg)
 , text "md5sum"
    <=> disp (arch_md5sum pkg)
 , hang
    (text "build() {") 4
             (vcat $ (map text) (arch_build pkg))
   $$ char '}'
 ]

--
-- | Tranlsate a generic cabal file into a PGKBUILD
--
cabal2pkg :: GenericPackageDescription -> PkgBuild

cabal2pkg GenericPackageDescription
    { packageDescription = cabal
    , genPackageFlags    = _cabal_genPackageFlags
    , condLibrary        = _cabal_condLibrary -- Nothing, if executables only
    , condExecutables    = _cabal_condExecutables
    }

--  = trace (show _cabal_condExecutables ) $
  =
   emptyPkgBuild
    { arch_pkgname = name
    , arch_pkgver  = vers
    , arch_url     = homepage cabal
    , arch_pkgdesc = synopsis cabal
    , arch_license = ArchList [license cabal]

    -- All Hackage packages depend on GHC at build time
    -- All Haskell libraries are prefixed with "haskell-"
    , arch_makedepends = (arch_makedepends emptyPkgBuild)
                            `mappend`
                         ArchList
                             [ ArchDep (Dependency ("haskell"++d) v)
                             | Dependency d v <- buildDepends cabal ]

    -- need the dependencies of all flags that are on by default, for all libraries and executables

    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/"
       ++ (name </> display vers </> name <-> display vers <.> "tar.gz")

    -- Generate the build/install script
    , arch_build =
        [ "cd $startdir/src/" </> name <-> display vers
        , "runhaskell Setup configure --prefix=/usr || return 1"
        , "runhaskell Setup build                   || return 1"
        , "runhaskell Setup register   --gen-script || return 1"
        , "runhaskell Setup unregister --gen-script || return 1"
        , "install -D -m744 register.sh   $startdir/pkg/usr/share/haskell/$pkgname/register.sh"
        , "install    -m744 unregister.sh $startdir/pkg/usr/share/haskell/$pkgname/unregister.sh"
        , "runhaskell Setup copy --destdir=$startdir/pkg || return 1"
        , "install -D -m644 " ++ licenseFile cabal ++
                             " $startdir/pkg/usr/share/licenses/$pkgname/LICENSE || return 1"
        ]

    }
  where
    name = pkgName (package cabal)
    vers = pkgVersion (package cabal)

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
    , arch_arch    :: ArchList Arch
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
        -- Build hooks

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
    , arch_arch        = ArchList [X86, X86_64]
    , arch_url         = homepage e
    , arch_license     = ArchList [license e]
    , arch_makedepends = ArchList [(ArchDep (Dependency "ghc" AnyVersion))]
    , arch_depends     = ArchList []
    , arch_source      = ArchList []
    , arch_md5sum      = ArchList []
    , arch_build       = []
    }
  where
    e = emptyPackageDescription

------------------------------------------------------------------------
-- Extra pretty printer instances and types

newtype ArchDep = ArchDep Dependency
  deriving (Eq,Show)

-- the PKGBUILD version spec is less expressive than cabal, we can't really handle
-- unions or intersections well yet.

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
      mydisp x = error $ "Can't handle this version format yet: " ++ show x

  parse = undefined

--
-- | Valid linux platforms
--
data Arch = X86 | X86_64
    deriving (Show, Eq)

instance Text Arch where
    disp x = case x of
       X86      -> text "i686"
       X86_64   -> text "x86_64"
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

