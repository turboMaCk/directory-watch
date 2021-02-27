{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Basic (spec) where

import Control.Monad.IO.Class (liftIO)

import Spec.Helper
import Test.Hspec

import Turtle ((</>))
import qualified Turtle as SH

import qualified System.Directory.Watch as Lib


spec :: Spec
spec = do
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

        it "Should be able to recursively start watching new directories" $ do
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
