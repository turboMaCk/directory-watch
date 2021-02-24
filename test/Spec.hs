{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.IO as IO

import Test.Hspec

import Turtle ((</>))
import qualified Turtle as SH
import qualified Turtle.Prelude as SH

import qualified System.Directory.Watch as Lib
import qualified System.Posix.Recursive as Recursive


type Events = [Lib.Event]


sleep :: MonadIO m => m ()
sleep = liftIO $ threadDelay 100_000


runWatch :: (SH.FilePath -> SH.Shell ()) -> IO Events
runWatch action = do
    events <- newIORef []

    SH.sh $ do
        workdir <- SH.mktempdir (SH.decodeString "test/workdir") "test"

        liftIO $ print workdir
        let sanitize event =
                let newPath = drop (length $ SH.encodeString workdir) $ Lib.filePath event
                 in event{Lib.filePath = newPath}

        liftIO $
            forkIO $
                Lib.withManager $ \manager -> do
                    Lib.watchDirectory manager $ SH.encodeString workdir

                    Lib.keepWatching manager $ \Lib.Event{..} -> do
                        modifyIORef' events $ (:) (sanitize Lib.Event{..})

                        case eventType of
                            Lib.DirectoryCreated -> pure () --Lib.watchDirectory manager filePath
                            Lib.FileCreated -> Lib.watchFile manager filePath
                            Lib.FileModified -> pure ()

        sleep
        action workdir
        sleep

    reverse <$> readIORef events


main :: IO ()
main = hspec $ do
    describe "Watcher observing turtle events" $ do
        it "Should trigger FileCreated" $ do
            events <- runWatch $ \wd -> do
                SH.touch (wd </> "foo")
                SH.touch (wd </> "bar")

            events
                `shouldBe` [ Lib.Event
                                { Lib.eventType = Lib.FileCreated
                                , Lib.filePath = "/foo"
                                }
                           , Lib.Event
                                { Lib.eventType = Lib.FileCreated
                                , Lib.filePath = "/bar"
                                }
                           ]

        it "Should trigger Directory Created" $ do
            events <- runWatch $ \wd -> do
                SH.mkdir (wd </> "new-dir")

            events
                `shouldBe` [ Lib.Event
                                { Lib.eventType = Lib.DirectoryCreated
                                , Lib.filePath = "/new-dir"
                                }
                           ]

        it "Should handle writing to file" $ do
            events <- runWatch $ \wd -> do
                SH.touch $ wd </> "baz"
                sleep
                liftIO $ SH.writeTextFile (wd </> "baz") "Hello there!"

            events
                `shouldBe` [ Lib.Event
                                { Lib.eventType = Lib.FileCreated
                                , Lib.filePath = "/baz"
                                }
                           , Lib.Event
                                { Lib.eventType = Lib.FileModified
                                , Lib.filePath = "/baz"
                                }
                           ]
