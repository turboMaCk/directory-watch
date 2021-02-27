{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Helper (
    shouldTrigger,
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (
    IORef,
    modifyIORef',
    newIORef,
    readIORef,
 )

import qualified Turtle as SH

import Test.Hspec (Expectation, shouldMatchList)

import qualified System.Directory.Watch as Lib


sleep :: MonadIO m => m ()
sleep = liftIO $ threadDelay 100_000


sanitize :: SH.FilePath -> Lib.Event -> Lib.Event
sanitize workdir event =
    let newPath = drop (length $ SH.encodeString workdir) $ Lib.filePath event
     in event{Lib.filePath = newPath}


shouldTrigger :: [Lib.Event] -> (SH.FilePath -> [SH.Shell ()]) -> Expectation
shouldTrigger expected action = do
    events <- newIORef []

    SH.sh $ do
        workdir <- SH.mktempdir (SH.decodeString "test/workdir") "test"

        liftIO $ putStrLn $ "Running test in dir " <> show workdir

        -- we will block on this
        done <- liftIO $ newEmptyMVar

        -- timeout
        _ <- liftIO $ forkIO $ threadDelay 1_000_000 *> putMVar done ()

        _ <- liftIO $
            forkIO $
                Lib.withManager $ \manager -> do
                    Lib.watchDirectory manager $ SH.encodeString workdir
                    handleEvents manager (length expected) done workdir events

        for_ (action workdir) $ \sh -> sleep *> sh

        liftIO $ takeMVar done

    result <- readIORef events
    result `shouldMatchList` expected
  where
    handleEvents :: Lib.Manager -> Int -> MVar () -> SH.FilePath -> IORef [Lib.Event] -> IO ()
    handleEvents manager n done workdir events
        | n > 0 =
            Lib.getEvent manager $ \Lib.Event{..} -> do
                modifyIORef' events $ (:) (sanitize workdir Lib.Event{..})

                case eventType of
                    Lib.DirectoryCreated -> Lib.watchDirectory manager filePath
                    Lib.FileCreated -> Lib.watchFile manager filePath
                    _ -> pure ()

                -- recurse
                handleEvents manager (n - 1) done workdir events
        | otherwise = putMVar done ()
