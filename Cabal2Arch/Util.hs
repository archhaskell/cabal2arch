-- |
-- Module    : Cabal2Arch.Util: utility functions for cabal2arch
-- Copyright : (c) Don Stewart, 2008 .. 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
-- Stability : provisional

module Cabal2Arch.Util where

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as CE

import System.Environment
import System.Exit
import System.IO
import System.Process hiding(cwd)

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
