{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void)
import Data.Foldable (for_)
import qualified System.Directory.Watch as Watch
import System.Environment (getArgs)
import qualified System.Posix.Files as Posix
import qualified System.Posix.Recursive as Recursive


main :: IO ()
main = do
    paths <- getArgs

    Watch.withManager $ \manager -> do
        for_ paths $ watchPath manager

        Watch.keepWatching manager $ \Watch.Event{..} -> do
            putStrLn $ "Event: " <> show Watch.Event{..}
            case eventType of
                Watch.MkDir ->
                    watchPath manager filePath

                _ ->
                    pure ()
  where
    watchPath manager path = do
        allDirs <- Recursive.listDirectories path

        for_ allDirs $ \path -> do
            Watch.watchTouch manager path
            Watch.watchMkDir manager path
