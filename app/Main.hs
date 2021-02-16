{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void)
import Data.Traversable (for)
import qualified System.Directory.Watch as Watch
import System.Environment (getArgs)
import qualified System.Posix.Files as Posix
import qualified System.Posix.Recursive as Recursive


main :: IO ()
main = do
    paths <- getArgs

    Watch.withManager $ \manager -> do
        for paths $ watchPath manager

        Watch.keepWatching manager $ \Watch.Event{..} -> do
            putStrLn $ "Event: " <> show Watch.Event{..}
            case eventType of
                Watch.MkDir ->
                    watchPath manager $ filePath
                _ ->
                    pure ()
  where
    watchPath manager path = do
        allDirs <- Recursive.listAccessible fsRecurseConf path

        void $
            for allDirs $ \path -> do
                Watch.watchTouch manager path
                Watch.watchMkDir manager path

    fsRecurseConf :: Recursive.Conf
    fsRecurseConf =
        Recursive.Conf
            { Recursive.preCheck = const True
            , Recursive.postCheck = \f _ -> Posix.isDirectory f
            , Recursive.followSymlinks = False
            }
