{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch.Backend.Inotify where

import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.HashMap.Strict as Map
import qualified System.Linux.Inotify as Inotify
import Prelude hiding (init)

import System.Directory.Watch.Portable


type Handle = Inotify.Inotify


type BackendEvent = Inotify.Event


type Id = Inotify.Watch


init :: IO Handle
init = Inotify.init
{-# INLINE init #-}


close :: Handle -> IO ()
close = Inotify.close
{-# INLINE close #-}


toEvent :: FilePath -> BackendEvent -> Event
toEvent path Inotify.Event{..} = Event{..}
  where
    filePath = path <> "/" <> Utf8.toString name
    eventType
        | Inotify.isSubset Inotify.in_ISDIR mask = MkDir
        | otherwise = Touch
{-# INLINE toEvent #-}


addTouch :: Handle -> FilePath -> IO Id
addTouch handle path =
    Inotify.addWatch handle path Inotify.in_MODIFY
{-# INLINE addTouch #-}


addMkDir :: Handle -> FilePath -> IO Id
addMkDir handle path =
    Inotify.addWatch handle path Inotify.in_CREATE
{-# INLINE addMkDir #-}


addBoth :: Handle -> FilePath -> IO Id
addBoth handle path =
    Inotify.addWatch handle path $ Inotify.in_CREATE `Inotify.isect` Inotify.in_MODIFY
{-# INLINE addBoth #-}


getEvent :: Handle -> IO BackendEvent
getEvent = Inotify.getEvent
{-# INLINE getEvent #-}


isDirectory :: BackendEvent -> Bool
isDirectory Inotify.Event{..} =
    Inotify.isSubset Inotify.in_ISDIR mask
{-# INLINE isDirectory #-}


getId :: BackendEvent -> Id
getId Inotify.Event{..} = wd
{-# INLINE getId #-}


internalWatch :: Handle -> FilePath -> IO Id
internalWatch handle path =
    Inotify.addWatch handle path Inotify.in_DELETE_SELF
{-# INLINE internalWatch #-}
