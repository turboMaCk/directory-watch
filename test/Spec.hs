{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Test.Hspec

import Turtle (liftIO, (</>))
import qualified Turtle as SH
import qualified Turtle.Prelude as SH

import qualified System.Directory.Watch as Lib


type Events = [Lib.Event]


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

                    Lib.keepWatching manager $ \event -> do
                        putStrLn $ "Event: " <> show event
                        modifyIORef' events $ (:) (sanitize event)

        liftIO $ threadDelay 1_000
        action workdir
        liftIO $ threadDelay 1_000

    reverse <$> readIORef events


main :: IO ()
main = hspec $ do
    describe "simple non recursive watcher" $ do
        it "Simple test" $ do
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
