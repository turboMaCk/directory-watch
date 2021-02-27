{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (
    IORef,
    modifyIORef',
    newIORef,
    readIORef,
 )
import System.IO as IO

import Test.Hspec

import Turtle ((</>))
import qualified Turtle as SH
import qualified Turtle.Prelude as SH

import qualified System.Directory.Watch as Lib


type Events = [Lib.Event]


sleep :: MonadIO m => m ()
sleep = liftIO $ threadDelay 100_000


sanitize :: SH.FilePath -> Lib.Event -> Lib.Event
sanitize workdir event =
    let newPath = drop (length $ SH.encodeString workdir) $ Lib.filePath event
     in event{Lib.filePath = newPath}


runWatch :: (SH.FilePath -> [SH.Shell ()]) -> IO Events
runWatch action = do
    events <- newIORef []

    SH.sh $ do
        workdir <- SH.mktempdir (SH.decodeString "test/workdir") "test"

        liftIO $ putStrLn $ "Running test in dir " <> show workdir

        liftIO $
            forkIO $
                Lib.withManager $ \manager -> do
                    Lib.watchDirectory manager $ SH.encodeString workdir

                    Lib.keepWatching manager $ \Lib.Event{..} -> do
                        modifyIORef' events $ (:) (sanitize workdir Lib.Event{..})

                        case eventType of
                            Lib.DirectoryCreated -> Lib.watchDirectory manager filePath
                            Lib.FileCreated -> Lib.watchFile manager filePath
                            _ -> pure ()

        for_ (action workdir) $ \sh -> sleep *> sh

        sleep

    -- longer sleep where we wait for extra events
    liftIO $ threadDelay 300_000
    reverse <$> readIORef events


shouldTrigger :: [Lib.Event] -> (SH.FilePath -> [SH.Shell ()]) -> Expectation
shouldTrigger expected action = do
    events <- newIORef []

    SH.sh $ do
        workdir <- SH.mktempdir (SH.decodeString "test/workdir") "test"

        liftIO $ putStrLn $ "Running test in dir " <> show workdir

        -- we will block on this
        done <- liftIO $ newEmptyMVar

        -- timeout
        liftIO $ forkIO $ threadDelay 1_000_000 *> putMVar done ()

        liftIO $
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


main :: IO ()
main = hspec $ do
    describe "Watcher observing turtle events" $ do
        it "Should trigger FileCreated" $
            shouldTrigger
                [ Lib.Event
                    { Lib.eventType = Lib.FileCreated
                    , Lib.filePath = "/foo"
                    }
                , Lib.Event
                    { Lib.eventType = Lib.FileCreated
                    , Lib.filePath = "/bar"
                    }
                ]
                $ \wd ->
                    [ SH.touch $ wd </> "foo"
                    , SH.touch $ wd </> "bar"
                    ]

        it "Should trigger Directory Created" $ do
            shouldTrigger
                [ Lib.Event
                    { Lib.eventType = Lib.DirectoryCreated
                    , Lib.filePath = "/new-dir"
                    }
                ]
                $ \wd ->
                    [SH.mkdir $ wd </> "new-dir"]

        it "Should handle writing to file" $ do
            shouldTrigger
                [ Lib.Event
                    { Lib.eventType = Lib.FileCreated
                    , Lib.filePath = "/baz"
                    }
                , Lib.Event
                    { Lib.eventType = Lib.FileModified
                    , Lib.filePath = "/baz"
                    }
                ]
                $ \wd ->
                    [ SH.touch $ wd </> "baz"
                    , liftIO $ SH.writeTextFile (wd </> "baz") "Hello there!"
                    ]

        it "should be able to recursively start watching new directories" $ do
            shouldTrigger
                [ Lib.Event
                    { Lib.eventType = Lib.DirectoryCreated
                    , Lib.filePath = "/sub-dir"
                    }
                , Lib.Event
                    { Lib.eventType = Lib.FileCreated
                    , Lib.filePath = "/sub-dir/foobar"
                    }
                , Lib.Event
                    { Lib.eventType = Lib.FileModified
                    , Lib.filePath = "/sub-dir/foobar"
                    }
                ]
                $ \wd ->
                    [ SH.mkdir $ wd </> "sub-dir"
                    , SH.touch $ wd </> "sub-dir" </> "foobar"
                    , liftIO $ SH.writeTextFile (wd </> "sub-dir" </> "foobar") "Hello there!"
                    ]
        it "Should not report more events than there should be" $ do
            events <- runWatch $ \wd ->
                [ SH.mkdir $ wd </> "sub-dir"
                , SH.touch $ wd </> "sub-dir" </> "foobar"
                , liftIO $ SH.writeTextFile (wd </> "sub-dir" </> "foobar") "Hello there!"
                ]

            events
                `shouldMatchList` [ Lib.Event
                                        { Lib.eventType = Lib.DirectoryCreated
                                        , Lib.filePath = "/sub-dir"
                                        }
                                  , Lib.Event
                                        { Lib.eventType = Lib.FileCreated
                                        , Lib.filePath = "/sub-dir/foobar"
                                        }
                                  , Lib.Event
                                        { Lib.eventType = Lib.FileModified
                                        , Lib.filePath = "/sub-dir/foobar"
                                        }
                                  , Lib.Event
                                        { Lib.eventType = Lib.FileRemoved
                                        , Lib.filePath = "/sub-dir/foobar"
                                        }
                                  , Lib.Event
                                        { Lib.eventType = Lib.DirectoryRemoved
                                        , Lib.filePath = "/sub-dir"
                                        }
                                  , Lib.Event
                                        { Lib.eventType = Lib.DirectoryRemoved
                                        , Lib.filePath = ""
                                        }
                                  ]
