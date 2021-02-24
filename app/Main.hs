{-# LANGUAGE RecordWildCards #-}

module Main where

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
                Watch.DirectoryCreated ->
                    watchPath manager filePath
                Watch.FileCreated ->
                    watchPath manager filePath
                _ ->
                    pure ()
  where
    watchPath manager path = do
        paths <-
            Recursive.listCustom
                Recursive.defConf
                    { Recursive.includeFile =
                        \stat _ -> pure (Posix.isRegularFile stat || Posix.isDirectory stat)
                    }
                path
        for_ paths $ Watch.watch manager
