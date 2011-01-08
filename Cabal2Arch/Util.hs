-- |
-- Module    : Cabal2Arch.Util: utility functions for cabal2arch
-- Copyright : (c) Don Stewart, 2008 .. 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
-- Stability : provisional

module Cabal2Arch.Util where

import Data.List

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as CE

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process hiding(cwd)

import Control.Monad.Trans
import Control.Monad.Error
import Distribution.ArchLinux.SystemProvides

import Paths_cabal2arch

type IOErr a = ErrorT String IO a

------------------------------------------------------------------------
-- Read a file from a URL
--
getFromURL :: String -> IOErr String
getFromURL url = do
   res <- liftIO (myReadProcess "curl" ["-f", url] "")
   case res of
     Left _ -> throwError ("Unable to retrieve " ++ url)
     Right s -> liftIO (return s)

-- Read from a file
getFromFile :: String -> IOErr String
getFromFile path = do
   b <- liftIO (doesFileExist path)
   if not b
     then throwError ("File " ++ path ++ " does not exist!")
     else liftIO (readFile path)

-- getDefaultSystemProvides = getSystemProvidesFromPath "http://andromeda.kiwilight.com/~remy.oudompheng/arch-haskell/default"
getDefaultSystemProvides = getSystemProvidesFromPath =<< (liftIO $ getDataFileName "data")

getSystemProvidesFromPath :: String -> IOErr SystemProvides
getSystemProvidesFromPath dir
  | null dir = getDefaultSystemProvides
  | "http://" `isPrefixOf` dir || "ftp://" `isPrefixOf` dir = do
     fc <- getFromURL (dir </> "ghc-provides.txt")
     fp <- getFromURL (dir </> "platform-provides.txt")
     ft <- getFromURL (dir </> "library-providers.txt")
     return (parseSystemProvides fc fp ft)
  | otherwise = do
     fc <- getFromFile (dir </> "ghc-provides.txt")
     fp <- getFromFile (dir </> "platform-provides.txt")
     ft <- getFromFile (dir </> "library-providers.txt")
     return (parseSystemProvides fc fp ft)

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
