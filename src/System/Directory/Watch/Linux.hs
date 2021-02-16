{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch.Linux (
    Id,
    Registery,
    InternalRegMap,
    Handle,
    initBackend,
    closeBackend,
    toEvent,
    addTouch,
    addMkDir,
    getBackendEvent,
    isDirectory,
    getId,
    internalWatch,
) where

import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.HashMap.Strict as Map
import qualified System.Linux.Inotify as Inotify

import System.Directory.Watch.Portable


type Registery = Map.HashMap Inotify.Watch FilePath


{- | In theory these 2 should aline in a way that
 3 in internal map will be 3 in the registery as well
 however this seems to be more robust especially since
 we allow for concurrency
-}
type InternalRegMap = Map.HashMap Inotify.Watch Inotify.Watch


type Handle = Inotify.Inotify


type BackendEvent = Inotify.Event


type Id = Inotify.Watch


initBackend :: IO Handle
initBackend = Inotify.init
{-# INLINE initBackend #-}


closeBackend :: Handle -> IO ()
closeBackend = Inotify.close
{-# INLINE closeBackend #-}


toEvent :: FilePath -> Inotify.Event -> Event
toEvent path Inotify.Event{..} = Event{..}
  where
    filePath = path <> "/" <> Utf8.toString name
    eventType
        | Inotify.isSubset Inotify.in_ISDIR mask = MkDir
        | otherwise = Touch
{-# INLINE toEvent #-}


addTouch :: Handle -> FilePath -> IO Inotify.Watch
addTouch handle path =
    Inotify.addWatch handle path Inotify.in_MODIFY
{-# INLINE addTouch #-}


addMkDir :: Handle -> FilePath -> IO Inotify.Watch
addMkDir handle path =
    Inotify.addWatch handle path Inotify.in_CREATE
{-# INLINE addMkDir #-}


getBackendEvent :: Handle -> IO BackendEvent
getBackendEvent = Inotify.getEvent
{-# INLINE getBackendEvent #-}


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
